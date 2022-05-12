(define-module (baba packages tailscale)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang))

(define-public go-software-sslmate-com-src-go-pkcs12
  (package
    (name "go-software-sslmate-com-src-go-pkcs12")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://software.sslmate.com/src/go-pkcs12.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r8hx5ply4c3jqkgaqlf4zsgg0rw54wbs2qlxf2m1skffb4gppj7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "software.sslmate.com/src/go-pkcs12"))
    (propagated-inputs `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/SSLMate/go-pkcs12")
    (synopsis "package pkcs12")
    (description
     "Package pkcs12 implements some of PKCS#12 (also known as P12 or PFX).  It is
intended for decoding DER-encoded P12/PFX files for use with the crypto/tls
package, and for encoding P12/PFX files for use by legacy applications which do
not support newer formats.  Since PKCS#12 uses weak encryption primitives, it
SHOULD NOT be used for new applications.")
    (license license:bsd-3)))

(define-public go-github-com-aws-aws-sdk-go-v2-service-sts
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-service-sts")
    (version "1.16.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/aws/aws-sdk-go-v2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1090xmqmryavl9j9p0qbv7n8lbhd7mpg56v2qskcn9cqhnz0bk7f"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "github.com/aws/aws-sdk-go-v2/service/sts"
        #:unpack-path
        "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs
      `(("go-github-com-aws-smithy-go" ,go-github-com-aws-smithy-go)))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis #f)
    (description
      "Package sts provides the API client, operations, and parameter types for AWS
Security Token Service.")
    (license license:asl2.0)))
