(define-module (baba packages linux)
  #:use-module (gnu packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (guix git-download)
  #:use-module (guix packages))

#|
guix repl -L . <<EOF
(use-modules (guix base32)
             (guix git)
             (guix git-download)
             (guix hash)
             (guix packages)
             (guix store)
             (baba packages linux))

(let* ((source (package-source linux-bcachefs))
       (url (git-reference-url (origin-uri source))))
  (call-with-values (lambda () (update-cached-checkout url))
    (lambda (path commit starting-commit?)
      (let ((hash (file-hash* path)))
        (format #t "commit: ~A~%hash: ~A~%" commit (bytevector->nix-base32-string hash))))))
EOF
|#

(define-public linux-bcachefs
  (package
    (inherit ((@@ (gnu packages linux) make-linux-libre*)
              linux-libre-version
              linux-libre-gnu-revision
              (origin
                (inherit linux-libre-5.17-source)
                (method git-fetch)
                (uri (git-reference
                      (url "https://evilpiepirate.org/git/bcachefs.git")
                      (commit "2013939f34fa35173ebcf82106c45e75b465f3ab")))
                (sha256
                 (base32 "1ls1b0j04ira5xpfgqcj26x9qf4gii9m4f2gpxwl21j7y77g0gf4"))
                (file-name (git-file-name "linux-bcachefs" linux-libre-version)))
              '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
              #:configuration-file (@@ (gnu packages linux) kernel-config)
              #:extra-options (acons "CONFIG_BCACHEFS_FS" 'm (@@ (gnu packages linux) %default-extra-linux-options))))
    (name "linux-testing")))
