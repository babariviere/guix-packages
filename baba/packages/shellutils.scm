(define-module (baba packages shellutils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public zsh-completions
  (package
    (name "zsh-completions")
    (version "0.34.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zsh-users/zsh-completions")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jjgvzj3v31yibjmq50s80s3sqi4d91yin45pvn3fpnihcrinam9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh-plugins
                     (string-append out "/share/zsh/plugins/zsh-completions"))
                    (src (string-append zsh-plugins "/src")))
               (mkdir-p zsh-plugins)
               (copy-file "zsh-completions.plugin.zsh" (string-append zsh-plugins "/zsh-completions.zsh"))
               (mkdir-p src)
               (copy-recursively "src" src)
               #t))))))
    (home-page "https://github.com/zsh-users/zsh-completions")
    (synopsis "Additional completion definitions for Zsh.")
    (description
     "This projects aims at gathering/developing new completion scripts that are not available in Zsh yet. The scripts may be contributed to the Zsh project when stable enough.")
    (license license:expat)))

(define-public zsh-z
  (package
    (name "zsh-z")
    (version "aaafebcd97424c570ee247e2aeb3da30444299cd")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/agkozak/zsh-z")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "147rwiqn5xs0vx7pkqvl1480s7fv7f5879cq6k42pn74jawzhspm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh-plugins
                     (string-append out "/share/zsh/plugins/zsh-z"))
                    (src (string-append zsh-plugins "/src")))
               (mkdir-p zsh-plugins)
               (copy-file "zsh-z.plugin.zsh" (string-append zsh-plugins "/zsh-z.zsh"))
               (copy-file "_zshz" (string-append zsh-plugins "/_zshz"))
               #t))))))
    (home-page "https://github.com/agkozak/zsh-z")
    (synopsis "Jump quickly to directories that you have visited \"frecently.\" A native Zsh port of z.sh with added features. ")
    (description
     "Jump quickly to directories that you have visited \"frecently.\" A native Zsh port of z.sh with added features. ")
    (license license:expat)))
