(define-module (baba packages tailscale)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_" version "_amd64.tgz"))
       (sha256
        (base32
         "1b697g694vigzmv5q48l1d3pjc9l5gwzazggnfi7z9prb9cvlnx2"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("tailscale" "bin/")
         ("tailscaled" "bin/"))))
    (home-page "https://tailscale.com")
    (synopsis #f)
    (description #f)
    (license #f)))
