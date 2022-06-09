(define-module (baba packages golang)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-golang-org-x-sys
  (package
    (name "go-golang-org-x-sys")
    (version "0.0.0-20220608164250-635b8c9b7f68")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/sys")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03b5khlhiyxk8xlhwfb768h6qqmgngl9jgj1yrc5nhr9jg901yv5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/sys"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://golang.org/x/sys")
    (synopsis "sys")
    (description
     "This repository holds supplemental Go packages for low-level interactions with
the operating system.")
    (license license:bsd-3)))

(define-public go-github-com-moby-sys-mountinfo
  (package
    (name "go-github-com-moby-sys-mountinfo")
    (version "0.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moby/sys")
                    (commit (string-append "mountinfo/v" (go-version->git-ref version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f4368vgz7fmm2fsdh4w9fnlzl3qbsmkj9nnm811s0l451pa8pqx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/moby/sys/mountinfo"
       #:unpack-path "github.com/moby/sys"
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))


(define-public go-github-com-moby-sys-mount
  (package
    (name "go-github-com-moby-sys-mount")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moby/sys")
                    (commit (string-append "mount/v" (go-version->git-ref version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "161dv3wwbh8zs2acfahzjmypb44aajd0dazh27fihn95h2kfzz6w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/moby/sys/mount"
       #:unpack-path "github.com/moby/sys"
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))
