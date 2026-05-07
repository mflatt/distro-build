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
         raco/cross
         "private/add-catalog.rkt"
         "private/find-matching.rkt"
         "private/pack-base64.rkt"
         "private/status.rkt"
         distro-build/installer
         distro-build/readme)

(provide build-catalog
         repackage)

(define (get-dirs)
  (define base-dir (path->complete-path (build-path "compiled" "repackage")))
  (define workspace-dir (build-path base-dir "workspace"))
  (define addon-dir (build-path base-dir "addon"))
  (define cache-dir (path->complete-path (build-path "compiled" "download-cache")))
  (values base-dir workspace-dir addon-dir cache-dir))

(define (build-catalog #:version version
                       #:packages packages
                       #:catalogs source-catalogs
                       #:original-prefix [original-prefix #f]
                       #:dest [dest "built"]
                       #:build-deps [build-deps '("draw-lib")]
                       #:fast? [fast? #f])
  (define-values (base-dir workspace-dir addon-dir cache-dir) (get-dirs))
  (define site-dir (build-path base-dir dest))

  (status "Working in ~a\n" workspace-dir)
  (make-directory* workspace-dir)

  (define (run #:any? [any? #t]
               #:quiet? [quiet? #f]
               command
               . args)
    (apply raco-cross
           #:workspace-dir workspace-dir
           #:compile-any? any?
           #:identity (and any? "catalog-builder")
           #:quiet? quiet?
           #:addon-dir addon-dir
           #:download-cache-dir cache-dir
           #:skip-pkgs? #true
           #:version version
           #:command command
           args))

  ;; make sure any needed foreign libraries are installed at host
  (apply run #:any? #f
         "pkg" "install" "--auto" "--skip-installed"
         build-deps)

  ;; create machine-indepenent instance
  (run "racket" "-n")

  ;; add new catalogs
  (add-catalogs run source-catalogs)

  ;; install in machine-independent cross target
  (apply run
         "pkg" "install" "-u" "--auto" "--skip-installed"
         packages)
  ;; In case we fixed something after a previous install
  (run "setup")

  (apply run "racket" (collection-file-path "make-catalog.rkt" "distro-build/private")
         (append
          (if original-prefix
              (list "--original-prefix" original-prefix)
              null)
          (list site-dir)
          packages)))

(define (repackage config
                   #:version version
                   #:file-name-version [file-name-version version]
                   #:catalogs [catalogs null]
                   #:version-note [version-note ""]
                   #:skip-notarize? [skip-notarize? #f])
  (define-values (base-dir workspace-dir addon-dir cache-dir) (get-dirs))
  (define readme-file (build-path base-dir "readme.txt"))
  (define installers-dir (build-path base-dir "build" "installers"))
  (define installer-table-file (build-path installers-dir "table.rktd"))
  (define cross-identity "repackaged")
  (define cross-dir (build-path workspace-dir cross-identity))

  (define installers-url "https://mirror.racket-lang.org/installers/9.1/")

  (define table-file (build-path base-dir "table.rktd"))
  (unless (file-exists? table-file)
    (make-directory* base-dir)
    (define u (combine-url/relative (string->url installers-url) "table.rktd"))
    (status "Getting table ~a\n" (url->string u))
    (define p (get-pure-port u))
    (call-with-output-file
     table-file
     (lambda (o) (copy-port p o)))
    (close-input-port p))
  (define table (file->value table-file))

  (define (build-one c #:just-plan? [just-plan? #f])
    (define name (hash-ref c '#:name #f))
    (define source? (let ([src? (hash-ref c '#:source? #f)])
                      (hash-ref c '#:source-runtime? src?)))
    (define target (or (hash-ref c '#:cross-target-machine #f)
                       (hash-ref c '#:cross-target #f)
                       (if source?
                           "source"
                           (format "~a-~a"
                                   (system-type 'arch)
                                   (if (and (hash-ref c '#:docker #f)
                                            (not (eq? (system-type) 'unix)))
                                       'linux
                                       (system-type 'os*))))))

    (define key (and name (find-matching name table)))
    (when key
      (status "~a ~a\n  <- ~a\n     ~a\n"
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
       (define (run #:quiet? [quiet? #f]
                    command . args)
         (unless quiet?
           (status "raco~a\n"
                   (apply string-append (map (lambda (v) (format " ~a" v))
                                             (cons command args)))))
         (apply raco-cross
                #:workspace-dir workspace-dir
                #:target target
                #:identity cross-identity
                #:addon-dir addon-dir
                #:download-cache-dir cache-dir
                #:version version
                #:quiet? quiet?
                #:skip-pkgs? #t
                #:compile-any? source?
                #:use-source? source?
                #:command command
                #:archive (hash-ref table key)
                args))

       ;; clean up, just in case there's a leftover after a previous error
       (delete-directory/files cross-dir #:must-exist? #f)

       (run "pkg" "config")

       (define orig-cat (add-catalogs run catalogs))

       (run "pkg" "config")

       (when source?
         ;; disable installation of any platform-specific packages
         (status "Set cross configuration in source\n")
         (define lib-dir (build-path cross-dir "lib"))
         (define sys-file (build-path lib-dir "system.rktd"))
         (make-directory* lib-dir)
         (raco-cross #:workspace-dir workspace-dir
                     #:addon-dir addon-dir
                     #:download-cache-dir cache-dir
                     #:version version
                     #:command "racket"
                     "-e"
                     (format "~s" `(begin
                                     (require setup/dirs)
                                     (copy-file (build-path (find-lib-dir) "system.rktd")
                                                ,(path->string sys-file)))))
         (let* ([ht (call-with-input-file* sys-file read)]
                [ht (hash-set ht 'library-subpath #"source")]
                #;
                [ht (hash-set ht 'target-machine #f)])
           (call-with-output-file*
            sys-file
            #:exists 'truncate
           (lambda (o)
             (writeln ht o)))))

       (apply run "pkg" "install" "-i" "--auto" "--skip-installed" "--recompile-only"
              (append
               (if (and source?
                        (hash-ref c '#:source-pkgs? (hash-ref c '#:source? #f)))
                   (list "--source" "--no-setup")
                   null)
               (hash-ref c '#:pkgs null)))

       (define short-human-name (hash-ref c '#:dist-name "Racket"))
       (define sign-identity (hash-ref c '#:sign-identity ""))
       (define sign-cert-config (hash-ref c '#:sign-cert-config #f))
       (define osslsigncode-args (hash-ref c '#:osslsigncode-args #f))
       (define notarization-config (hash-ref c '#:notarization-config #f))
       (define release? (hash-ref c '#:release? #f))
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

       (status "Reset configuration\n")
       (let ()
         (define config-file (build-path cross-dir "etc" "config.rktd"))
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
       
       (status "Clean build directory\n")
       (delete-directory/files (build-path cross-dir "build")
                               #:must-exist? #f)

       (status "Generating README\n")
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
       ;; remove existing README, in case it uses a different extension convention
       (for ([readme (in-list '("README" "README.txt"))])
         (delete-directory/files (build-path cross-dir readme) #:must-exist? #f))

       (when (and source?
                  (hash-ref c '#:source-pkgs? (hash-ref c '#:source? #f)))
         ;; For an original disto build, this step is performed by
         ;; `setup/unixstyle-install post-adjust --source`, but since we
         ;; started with a source distribution, the only thing that needs to
         ;; be fixed up is removing compiled files
         (status "Clean compiled directories\n")
         (for ([p (in-directory (build-path cross-dir "collects")
                                (lambda (p)
                                  (define-values (base name dir?) (split-path p))
                                  (not (equal? (path->string name) "compiled"))))])
           (define-values (base name dir?) (split-path p))
           (when (equal? (path->string name) "compiled")
             (delete-directory/files p))))

       (parameterize ([current-directory base-dir])
         (delete-directory/files "bundle" #:must-exist? #f)
         (make-directory* "bundle")
         (printf "Packing\n")
         (flush-output)
         (define (maybe-add-version s add? version) (if add? (string-append s "-" version) s))
         (define (config-paths-to-strings ht) (for/hash ([(k v) (in-hash ht)])
                                                (values k (if (path? v)
                                                              (path->string v)
                                                              v))))
         (installer #:short-human-name short-human-name
                    #:human-name (format "~a v~a" short-human-name version)
                    #:base-name (maybe-add-version (hash-ref c '#:dist-base "racket")
                                                   (not versionless?)
                                                   file-name-version)
                    #:dir-name (maybe-add-version (hash-ref c '#:dist-dir "racket")
                                                  (not (or (and release? (not source?))
                                                           versionless?))
                                                  version)
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
                                           (pack-base64-strings
                                            (config-paths-to-strings
                                             sign-cert-config))
                                           "")
                    #:release? release? 
                    #:source? source?
                    #:versionless? versionless?
                    #:tgz? (hash-ref c '#:tgz? #f)
                    #:mac-pkg? (hash-ref c '#:mac-pkg? #f)
                    #:hardened-runtime? (hash-ref c '#:hardened-runtime? (not (equal? sign-identity "")))
                    #:notarization-config (and notarization-config
                                               (not skip-notarize?)
                                               (pack-base64-strings
                                                (config-paths-to-strings notarization-config)))
                    #:download-readme (url->string (path->url readme-file))
                    #:pre-process-cmd (let ([p (hash-ref c '#:client-installer-pre-process '())])
                                         (and (pair? p)
                                              (pack-base64-strings p)))
                    #:post-process-cmd (let ([p (hash-ref c '#:client-installer-post-process '())])
                                         (and (pair? p)
                                              (pack-base64-strings p)))
                    #:dist-base-version version
                    #:platform (normalize-platform target)
                    #:cross-system-type cross-system-type
                    #:src-dir cross-dir))

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
       (delete-directory/files cross-dir)]))

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
              #:download-cache-dir cache-dir
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
   (define config
     (parameterize ([current-mode (or config-mode "default")])
       (dynamic-require (path->complete-path config-file) 'site-config)))
   (repackage config
              #:version vers
              #:version-note vers-note
              #:catalogs (reverse rev-catalogs))))
