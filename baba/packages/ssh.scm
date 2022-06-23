(define-module (baba packages ssh)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


(define-public x11-ssh-askpass
  (package
    (name "x11-ssh-askpass")
    (version "1.2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pkgs.fedoraproject.org/repo/pkgs/openssh/x11-ssh-askpass-" version
                                  ".tar.gz/8f2e41f3f7eaa8543a2440454637f3c3/x11-ssh-askpass-" version ".tar.gz"))
              (sha256
               (base32 "124c1frwvdmg4nv8xqv435ibjhj2y8xc1bmfr6i8a8g75b1y63b2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CDEBUGFLAGS=")
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure" (string-append "--prefix=" out)
                       (string-append "--with-app-defaults-dir=" out "/etc/X11/app-defaults")))))
         (add-after 'configure 'xmkmf
           (lambda _
             (invoke "xmkmf")
             (invoke "make" "includes"))))))
    (inputs (list libxt))
    (native-inputs (list autoconf automake imake gccmakedep))
    (home-page "https://github.com/sigmavirus24/x11-ssh-askpass")
    (synopsis #f)
    (description #f)
    (license license:expat)))
