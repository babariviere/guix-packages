(define-module (baba packages firmware)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages textutils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public fwup
  (package
    (name "fwup")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fwup-home/fwup.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "00rq233ff1pv5hpj0ksd55cbgap2bk7aaynisf1gi8vsvadh2701"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let* ((zlib (assoc-ref %build-inputs "zlib"))
              (zstd (assoc-ref %build-inputs "zstd"))
              (bzip2 (assoc-ref %build-inputs "bzip2")))
         `(,(string-append "LDFLAGS="
                           "-Wl,-rpath=" zlib "/lib "
                           "-Wl,-rpath=" zstd "/lib "
                           "-Wl,-rpath=" bzip2 "/lib ")))))
    (inputs (list bzip2
                  libconfuse
                  libarchive
                  libtool
                  dosfstools
                  libtool
                  pkg-config
                  xdelta
                  zlib
                  `(,zstd "lib")))
    (native-inputs (list autoconf automake pkg-config mtools which))
    (propagated-inputs (list binutils unzip zip))
    (home-page "https://github.com/fwup-home/fwup")
    (synopsis "Configurable embedded Linux firmware update creator and runner")
    (description "fwup is a configurable image-based software update utility for embedded Linux-based systems.
It primarily supports software upgrade strategies that update entire root filesystem images at once. This includes strategies like swapping back and forth between A and B partitions, recovery partitions, and various trial update/failback scenarios.")
    (license license:expat)))
