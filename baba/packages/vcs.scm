(define-module (baba packages vcs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages node)
  #:use-module (gnu packages version-control))

(define-public gitea
  (package
    (name "gitea")
    (version "1.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/go-gitea/gitea/releases"
                                  "/download/v" version
                                  "/gitea-src-" version ".tar.gz"))
              (sha256
               (base32 "0nkv5a49iryx806fba76l8y7pbx0vir7npf6hf53wc4zaqk0x2i0"))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'unpatch-example-shebangs
           ;; If we don't do this then git repos created with this version of
           ;; gitea will use the build environment's bash for the different
           ;; git repo hooks.
           (lambda _
             (substitute*
               (find-files "src/integrations/gitea-repositories-meta"
                           "(\\.sample|gitea|(post|pre)-receive|update)")
               (("#!/gnu/store/.*/bin/bash") "#!/usr/bin/env bash")
               (("#!/gnu/store/.*/bin/sh") "#!/bin/sh"))))
         (add-before 'build 'prepare-build
           (lambda _
             (setenv "TAGS" "bindata sqlite sqlite_unlock_notify")
             #t))
         (replace 'build
           (lambda _
             (with-directory-excursion "src"
               (invoke "make" "build"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src"
                 (invoke "make" "test")
                 ;; Gitea requires git with lfs support to run tests.
                 ;(invoke "make" "test-sqlite")
                 (invoke "make" "test-sqlite-migration")))))
         (replace 'install
           (lambda _
             (with-directory-excursion "src"
               (invoke "make" "install"))))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/gitea"))
                    (git (assoc-ref inputs "git")))
               (wrap-program bin
                 `("PATH" ":" prefix (,(string-append git "/bin")))))
             #t)))))
    (native-inputs
     `(("node" ,node)))
    (inputs
     `(("bash" ,bash-minimal)
       ("git" ,git)))
    (home-page "https://gitea.io/")
    (synopsis "Self-hosted git service")
    ;; TODO: Rewrite description
    (description "Gitea is a painless self-hosted Git service.  It is similar
to GitHub, Bitbucket, and GitLab.")
    (properties
      '((release-monitoring-url . "https://github.com/go-gitea/gitea/releases")))
    (license license:expat)))
