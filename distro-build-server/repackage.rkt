#lang racket/base
(require net/url
         racket/port
         racket/file
         racket/path
         (only-in distro-build/config
                  current-mode
                  site-config?
                  site-config-tag site-config-options site-config-content
                  merge-options
                  current-stamp
                  compose-aliases
                  get-client-name)
         "private/add-catalog.rkt"
         "private/find-matching.rkt"
         "private/pack-base64.rkt"
         distro-build/installer
         distro-build/readme)

(define (repackage config
                   #:version version
                   #:catalogs [catalogs null]
                   #:version-note [version-note ""])
  (define raco-cross (dynamic-require 'raco/cross 'raco-cross))
  (define normalize-platform (dynamic-require 'raco/cross 'normalize-platform))
  (define base-dir (path->complete-path (build-path "compiled" "repackage")))
  (define workspace-dir (build-path base-dir "workspace"))
  (define addon-dir (build-path base-dir "addon"))
  (define readme-file (build-path base-dir "readme.txt"))
  (define installers-dir (build-path base-dir "build" "installers"))
  (define installer-table-file (build-path installers-dir "table.rktd"))

  (define installers-url "https://mirror.racket-lang.org/installers/9.1/")

  (define table-file (build-path base-dir "table.rktd"))
  (unless (file-exists? table-file)
    (make-directory* base-dir)
    (define u (combine-url/relative (string->url installers-url) "table.rktd"))
    (printf "Getting table ~a\n" (url->string u))
    (define p (get-pure-port u))
    (call-with-output-file
     table-file
     (lambda (o) (copy-port p o)))
    (close-input-port p))
  (define table (file->value table-file))

  (define (build-one c #:just-plan? [just-plan? #f])
    (define name (hash-ref c '#:name #f))
    (define source? (let ([src? (hash-ref c '#:source? #f)])
                      (or (hash-ref c '#:source-runtime? src?))))
    (define target (or (hash-ref c '#:cross-target-machine #f)
                       (hash-ref c '#:cross-target #f)
                       (format "~a-~a"
                               (system-type 'arch)
                               (if (and (hash-ref c '#:docker #f)
                                        (not (eq? (system-type) 'unix)))
                                   'linux
                                   (system-type 'os*)))))

    (define key (and name (find-matching name table)))
    (when key
      (printf "~a ~a\n  <- ~a\n     ~a\n"
              (if just-plan? "----" "====")
              name
              (or key "SKIP")
              (if source? "source" (format "~a = ~a" target (normalize-platform target)))))

    (define installer-table (if (file-exists? installer-table-file)
                                (file->value installer-table-file)
                                (hash)))
    
    (cond
      [(or just-plan? (not key))
       (void)]
      [(hash-ref installer-table name #f)
       ;; installer already built
       (void)]
      [else
       (when source? (exit 0))
       
       (define (run #:quiet? [quiet? #f]
                    command . args)
         (unless quiet?
           (printf "raco~a\n"
                   (apply string-append (map (lambda (v) (format " ~a" v)) args))))
         (apply raco-cross
                #:workspace-dir workspace-dir
                #:target target
                #:identity "repackaged"
                #:addon-dir addon-dir
                #:version version
                #:quiet? quiet?
                #:skip-pkgs? #t
                #:command command
                #:archive (hash-ref table key)
                args))

       (run "pkg" "config")

       (define orig-cat (add-catalogs run catalogs))

       (run "pkg" "config")

       (apply run "pkg" "install" "-i" "--auto" "--skip-installed" "--recompile-only"
              (hash-ref c '#:pkgs null))

       (define short-human-name (hash-ref c '#:dist-name "Racket"))
       (define sign-identity (hash-ref c '#:sign-identity ""))
       (define sign-cert-config (hash-ref c '#:sign-cert-config #f))
       (define osslsigncode-args (hash-ref c '#:osslsigncode-args #f))
       (define notarization-config (hash-ref c '#:notarization-config #f))
       (define release? (hash-ref c '#:release? #t))
       (define versionless? (hash-ref c '#:versionless? #f))
       (define install-name (hash-ref c '#:install-name ""))
       (define cross-system-type (or (hash-ref c '#:target-platform #f)
                                     (cond
                                       [(regexp-match? #rx"osx" target) 'macosx]
                                       [(regexp-match? #rx"win32|(nt$)" target) 'windows]
                                       [else 'unix])))
       (define doc-search-url (or (hash-ref c '#:doc-search-url #f)
                                  (let ([v (hash-ref c '#:dist-base-url #f)])
                                    (and v
                                         (url->string
                                          (combine-url/relative (string->url v) "doc/local-redirect/index.html"))))))

       (printf "Reset configuration\n")
       (let ()
         (define config-file (build-path workspace-dir "repackaged" "etc" "config.rktd"))
         (let* ([ht (file->value config-file)]
                [ht (hash-remove ht 'default-scope)]
                [ht (if (equal? install-name "")
                        (hash-remove ht 'installation-name)
                        (hash-set ht 'installation-name install-name))]
                [ht (if doc-search-url
                        (hash-set ht 'doc-search-url doc-search-url)
                        ht)])
           (call-with-output-file*
            config-file
            #:exists 'truncate
            (lambda (o) (writeln ht o)))))

       (printf "Generating README\n")
       (flush-output)
       (let ([readme (make-readme
                      (hash '#:name name
                            '#:version version
                            '#:stamp (string-append "" version-note)
                            '#:dist-catalogs (cons orig-cat catalogs)
                            '#:versionless? versionless?
                            '#:install-name install-name
                            '#:target-platform cross-system-type))])
         (call-with-output-file*
          readme-file
          #:exists 'truncate
          (lambda (o) (display readme o))))

       (parameterize ([current-directory base-dir])
         (delete-directory/files "bundle" #:must-exist? #f)
         (make-directory* "bundle")
         (printf "Packing\n")
         (flush-output)
         (installer #:short-human-name short-human-name
                    #:human-name (format "~a v~a" short-human-name version)
                    #:base-name (hash-ref c '#:dist-base "racket")
                    #:dir-name (hash-ref c '#:dist-dir "racket")
                    #:dist-suffix (let ([s1 (hash-ref c '#:dist-suffix "")]
                                        [s2 (hash-ref c '#:dist-vm-suffix "")])
                                    (define s
                                      (cond
                                        [(equal? s1 "") s2]
                                        [(equal? s2 "") s1]
                                        [else (string-append s1 "-" s2)]))
                                    (if (equal? s "")
                                        ""
                                        (string-append "-" s)))
                    #:sign-identity sign-identity
                    #:osslsigncode-args-base64 (if osslsigncode-args
                                                   (pack-base64-strings osslsigncode-args)
                                                   "")
                    #:sign-cert-base64 (if sign-cert-config
                                           (pack-base64-strings sign-cert-config)
                                           "")
                    #:release? release? 
                    #:source? source?
                    #:versionless? versionless?
                    #:tgz? (hash-ref c '#:tgz? #f)
                    #:mac-pkg? (hash-ref c '#:mac-pkg? #f)
                    #:hardened-runtime? (hash-ref c '#:hardened-runtime? (not (equal? sign-identity "")))
                    #:notarization-config (and notarization-config
                                               (pack-base64-strings notarization-config))
                    ;; #:download-readme [download-readme #f]
                    ;; #:post-process-cmd [post-process-cmd #f]
                    ;; #:pre-process-cmd [pre-process-cmd #f]
                    #:dist-base-version version
                    #:platform (normalize-platform target)
                    #:cross-system-type cross-system-type
                    #:src-dir (build-path workspace-dir "repackaged")))

       (define result-name
         (let ([inst (build-path base-dir "bundle" "installer.txt")])
           (and (file-exists? inst)
                (call-with-input-file inst read-line))))
       (cond
         [result-name
          (printf "Registering result ~a\n" result-name)
          (make-directory* installers-dir)
          (rename-file-or-directory (build-path base-dir result-name)
                                    (build-path installers-dir (file-name-from-path result-name)))
          (call-with-output-file*
           installer-table-file
           #:exists 'truncate
           (lambda (o)
             (write (hash-set installer-table name result-name) o)))]
         [else
          (printf "FAILED ~s\n" name)])

       (printf "Removing cross directory\n")
       (delete-directory/files (build-path workspace-dir "repackaged"))]))

  (define (build just-plan?)
    (let loop ([config config]
               [opts (hasheq)])
      (case (site-config-tag config)
        [(parallel sequential)
         (define new-opts (merge-options opts config))
         (for-each (lambda (c) (loop c new-opts))
                   (site-config-content config))]
        [else
         (define c (merge-options opts config))
         (when (hash-ref c '#:name #f)
           (build-one c #:just-plan? just-plan?))])))

  (build #t)

  ;; make sure needed libraries are available
  (printf "Preparing native\n")
  (raco-cross #:workspace-dir workspace-dir
              #:addon-dir addon-dir
              #:version version
              #:command "pkg"
              "install" "--auto" "--skip-installed" "draw-lib")

  (build #f))

(module+ main
  (require racket/cmdline)
  (define vers (version))
  (define vers-note "")
  (define rev-catalogs null)
  (define config-mode #f)
  (command-line
   #:once-each
   [("--version") version "Racket <version>"
                  (set! vers version)]
   [("--version-note") note "Add <note> to README"
                       (set! vers-note note)]
   [("--mode") mode "Provide <mode> to configuration"
               (set! config-mode mode)]
   #:multi
   [("++catalog") catalog "Add <catalog>"
                  (set! rev-catalogs (cons catalog rev-catalogs))]
   #:args (config-file)
   (when config-mode (current-mode config-mode))     
   (repackage (dynamic-require `(file ,config-file) 'site-config)
              #:version vers
              #:version-note vers-note
              #:catalogs (reverse rev-catalogs))))
