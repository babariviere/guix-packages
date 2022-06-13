(define-module (baba packages pingu)
  #:use-module (baba packages golang)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define with-latest-x-sys
  (package-input-rewriting/spec `(("go-golang-org-x-sys" . ,(const go-golang-org-x-sys)))))

(define-public pingu
  (with-latest-x-sys
   (package
     (name "pingu")
     (version "0.0.2")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/sheepla/pingu")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1cppwyw7j2l1m7d18gij3rljlbr2pvqh8m13z0dszwpcfqrffpan"))))
     (build-system go-build-system)
     (arguments
      `(#:import-path "github.com/sheepla/pingu"
        #:go ,go-1.18))
     (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)
                          ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                          ("go-golang-org-x-net" ,go-golang-org-x-net)
                          ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
                          ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
                          ("go-github-com-google-uuid" ,go-github-com-google-uuid)
                          ("go-github-com-jessevdk-go-flags" ,go-github-com-jessevdk-go-flags)
                          ("go-github-com-go-ping-ping" ,go-github-com-go-ping-ping)
                          ("go-github-com-fatih-color" ,go-github-com-fatih-color)))
     (home-page "https://github.com/sheepla/pingu")
     (synopsis "🐧 pingu")
     (description
      "Simply specify the target host name or IP address in the first argument.")
     (license license:expat))))
