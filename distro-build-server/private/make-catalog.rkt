#lang racket/base
(require pkg/lib
         (prefix-in db: pkg/db)
         racket/cmdline
         racket/file
         net/url
         setup/getinfo)

;;  This module is run in a cross environment to build packages

(define original-repo-prefix #f)

(define-values (site-dir local-packages)
  (command-line
   #:once-each
   [("--original-prefix") prefix "Set original repo using <prefix>"
                          (set! original-repo-prefix prefix)]
   #:args (site-dir . local-package)
   (values site-dir local-package)))

(define addon-dir (find-system-path 'addon-dir))
(define dest-dir (build-path site-dir "pkgs"))

(define tmp-catalog (build-path site-dir "pkgs.sqlite"))
(define dest-catalog (build-path site-dir "catalog"))

(define default-author "plt@racket-lang.org")

;; catalog to consult for non-local package details, such as the author
(define truth-cat "https://pkgs.racket-lang.org")

;; Get all package names that are from the main catalog; we don't want to
;; provide these
(define main-cat (for/or ([cat (in-list (pkg-config-catalogs))])
                   (and (not (equal? (url-scheme (string->url cat)) "file"))
                        cat)))
(printf "Main catalog: ~a\n" main-cat)
(define main-pkgs
  (parameterize ([current-pkg-catalogs (list (string->url main-cat))])
    (for/hash ([name (in-list (get-all-pkg-names-from-catalogs))])
      (values name #f))))

(define pkg-details
  (parameterize ([current-pkg-catalogs (list (string->url truth-cat))])
    (get-all-pkg-details-from-catalogs)))

(define installed-pkgs
  (for/hash ([pkg (in-list (installed-pkg-names #:scope 'user))])
    (values pkg #t)))

(define catalog-pkgs
  (for/fold ([ht installed-pkgs]) ([k (in-hash-keys main-pkgs)])
    (hash-remove ht k)))

(printf "Packages to catalog:\n")
(for ([k (in-hash-keys catalog-pkgs)])
  (printf "  ~a\n" k))

;; We'd like to use `pkg-archive`, but it doesn't support
;; a stripping mode (which needs to be 'built) as of v9.2. Also,
;; we want to specify an `#:original` URL to better support
;; `raco pkg update --clone`

(define cache (make-hash))

(parameterize ([db:current-pkg-catalog-file tmp-catalog])
  (db:set-catalogs! (list "local"))
  (db:set-pkgs! "local" (hash-keys catalog-pkgs)))

(make-directory* dest-dir)
(for ([name (in-hash-keys catalog-pkgs)])
  (define pkg-dir (pkg-directory name #:cache cache))
  (define info (get-info/full pkg-dir))

  (define (extract-pkg p) (if (pair? p) (car p) p))

  (define deps (map extract-pkg (info 'deps (lambda () null))))
  (define build-deps (map extract-pkg (info 'build-deps (lambda () null))))

  (define mod-paths (pkg-directory->module-paths pkg-dir name))

  (define details (hash-ref pkg-details name (hash)))

  (define local? (and (member name local-packages) #t))
  
  (define author
    (if (not local?)
        (hash-ref details 'author default-author)
        (let ([author (info 'author (lambda () default-author))])
          (if (symbol? author)
              (format "~a@racket-lang.org" author)
              author))))
  (define desc
    (if (not local?)
        (hash-ref details 'description "")
        (info 'pkg-desc (lambda () ""))))

  (printf "~a by ~a: ~a\n" name author desc)

  (pkg-create 'zip
              pkg-dir
              #:dest dest-dir
              #:mode 'built
              #:original (if (not local?)
                             (hash-ref details 'source #f)
                             (and original-repo-prefix
                                  (string-append original-repo-prefix name))))

  (define source-file (build-path dest-dir (string-append name ".zip")))
  (define checksum (file->string (build-path dest-dir (string-append name ".zip.CHECKSUM"))))

  (parameterize ([db:current-pkg-catalog-file tmp-catalog])
    (db:set-pkg! name "local"
                 author
                 (path->string source-file)
                 checksum
                 desc)
    (db:set-pkg-dependencies! name "local"
                              checksum
                              (hash-keys
                               (for/hash ([k (in-list (append deps build-deps))])
                                 (values k #t))))
    (db:set-pkg-modules! name "local"
                         checksum
                         mod-paths))

  (pkg-catalog-copy (list tmp-catalog)
                    dest-catalog
                    #:force? #t
                    #:override? #t
                    #:relative-sources? #true))

(delete-file tmp-catalog)
