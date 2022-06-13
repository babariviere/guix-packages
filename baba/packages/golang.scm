(define-module (baba packages golang)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:replace (go-golang-org-x-sys))

(define-public go-1.18
  (package
    (inherit go-1.17)
    (name "go")
    (version "1.18.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/go")
                    (commit (string-append "go" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16h9776nzbhvgr86kv6q5phcxqg9566b3gv7kil80ybdyszm3kl1"))))
        (arguments
     `(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))
       ;; TODO: Disable the test(s) in misc/cgo/test/cgo_test.go
       ;; that cause segfaults in the test suite.
       #:tests? ,(not (target-aarch64?))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((output (assoc-ref outputs "out"))
                   (loader (string-append (assoc-ref inputs "libc")
                                          ,(glibc-dynamic-linker))))
               (setenv "GOOS" "linux")
               (setenv "GO_LDSO" loader)
               (setenv "GOROOT" (getcwd))
               (setenv "GOROOT_FINAL" (string-append output "/lib/go"))
               (setenv "GOGC" "400")
               (setenv "GOCACHE" "/tmp/go-cache"))))
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((net-base (assoc-ref inputs "net-base"))
                    (tzdata-path (string-append (assoc-ref inputs "tzdata")
                                                "/share/zoneinfo")))
               ;; XXX: Remove when #49729 is merged?
               (for-each make-file-writable (find-files "src"))

               ;; Having the patch in the 'patches' field of <origin> breaks
               ;; the 'TestServeContent' test due to the fact that
               ;; timestamps are reset.  Thus, apply it from here.
               (invoke "patch" "-p1" "--force" "-i"
                       (assoc-ref inputs "go-skip-gc-test.patch"))
               (invoke "patch" "-p1" "--force" "-i"
                       (assoc-ref inputs "go-fix-script-tests.patch"))

               (substitute* "src/os/os_test.go"
                 (("/usr/bin") (getcwd))
                 (("/bin/sh") (which "sh")))

               (substitute* "src/cmd/go/testdata/script/cgo_path_space.txt"
                 (("/bin/sh") (which "sh")))

               ;; fix shebang for testar script
               ;; note the target script is generated at build time.
               (substitute* "misc/cgo/testcarchive/carchive_test.go"
                 (("/usr/bin/env bash") (which "bash")))

               (substitute* "src/net/lookup_unix.go"
                 (("/etc/protocols")
                  (string-append net-base "/etc/protocols")))
               (substitute* "src/net/port_unix.go"
                 (("/etc/services")
                  (string-append net-base "/etc/services")))
               (substitute* "src/time/zoneinfo_unix.go"
                 (("/usr/share/zoneinfo/") tzdata-path)))))
         ;; Keep this synchronized with the package inputs.
         ,@(if (target-arm?)
             '((add-after 'unpack 'patch-gcc:lib
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((gcclib (string-append (assoc-ref inputs "gcc:lib") "/lib")))
                     ;; Add libgcc to runpath
                     (substitute* "src/cmd/link/internal/ld/lib.go"
                       (("!rpath.set") "true"))
                     (substitute* "src/cmd/go/internal/work/gccgo.go"
                       (("cgoldflags := \\[\\]string\\{\\}")
                        (string-append "cgoldflags := []string{"
                                       "\"-Wl,-rpath=" gcclib "\""
                                       "}"))
                       (("\"-lgcc_s\", ")
                        (string-append
                         "\"-Wl,-rpath=" gcclib "\", \"-lgcc_s\", ")))
                     (substitute* "src/cmd/go/internal/work/gc.go"
                       (("ldflags = setextld\\(ldflags, compiler\\)")
                        (string-append
                         "ldflags = setextld(ldflags, compiler)\n"
                         "ldflags = append(ldflags, \"-r\")\n"
                         "ldflags = append(ldflags, \"" gcclib "\")\n")))))))
             '())
         (add-after 'patch-source 'disable-failing-tests
           (lambda _
             ;; Disable failing tests: these tests attempt to access
             ;; commands or network resources which are neither available
             ;; nor necessary for the build to succeed.
             (for-each
              (match-lambda
                ((file test)
                 (let ((regex (string-append "^(func\\s+)(" test "\\()")))
                   (substitute* file
                     ((regex all before test_name)
                      (string-append before "Disabled" test_name))))))
              '(("src/net/cgo_unix_test.go" "TestCgoLookupPort")
                ("src/net/cgo_unix_test.go" "TestCgoLookupPortWithCancel")
                ;; 127.0.0.1 doesn't exist
                ("src/net/cgo_unix_test.go" "TestCgoLookupPTR")
                ("src/net/cgo_unix_test.go" "TestCgoLookupPTRWithCancel")
                ;; /etc/services doesn't exist
                ("src/net/parse_test.go" "TestReadLine")
                ;; The user's directory doesn't exist
                ("src/os/os_test.go" "TestUserHomeDir")
                ("src/cmd/link/internal/ld/fallocate_test.go" "TestFallocate")))

             ;; These tests fail on aarch64-linux
             (substitute* "src/cmd/dist/test.go"
               (("t.registerHostTest\\(\"testsanitizers/msan.*") ""))))
         (add-after 'patch-source 'enable-external-linking
           (lambda _
             ;; Invoke GCC to link any archives created with GCC (that is, any
             ;; packages built using 'cgo'), because Go doesn't know how to
             ;; handle the runpaths but GCC does.  Use substitute* rather than
             ;; a patch since these files are liable to change often.
             ;;
             ;; XXX: Replace with GO_EXTLINK_ENABLED=1 or similar when
             ;; <https://github.com/golang/go/issues/31544> and/or
             ;; <https://github.com/golang/go/issues/43525> are resolved.
             (substitute* "src/cmd/link/internal/ld/config.go"
               (("iscgo && externalobj") "iscgo"))
             (substitute* '("src/cmd/nm/nm_cgo_test.go"
                            "src/cmd/dist/test.go")
               (("^func.*?nternalLink\\(\\).*" all)
                (string-append all "\n\treturn false\n")))))
         (replace 'build
           (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
             (let* ((njobs (if parallel-build? (parallel-job-count) 1)))
               (with-directory-excursion "src"
                 (setenv "GOMAXPROCS" (number->string njobs))
                 (invoke "sh" "make.bash" "--no-banner")))))
         (replace 'check
           (lambda* (#:key target (tests? (not target)) (parallel-tests? #t)
                     #:allow-other-keys)
             (let* ((njobs (if parallel-tests? (parallel-job-count) 1)))
               (when tests?
                 (with-directory-excursion "src"
                   (setenv "GOMAXPROCS" (number->string njobs))
                   (invoke "sh" "run.bash" "--no-rebuild"))))))
         (add-before 'install 'unpatch-perl-shebangs
           (lambda _
             ;; Avoid inclusion of perl in closure by rewriting references
             ;; to perl input in sourcecode generators and test scripts
             (substitute* (cons "src/net/http/cgi/testdata/test.cgi"
                                (find-files "src" "\\.pl$"))
               (("^#!.*") "#!/usr/bin/env perl\n"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Notably, we do not install archives (180M), which Go will
             ;; happily recompile quickly (and cache) if needed, almost
             ;; surely faster than they could be substituted.
             ;;
             ;; The main motivation for pre-compiled archives is to use
             ;; libc-linked `net' or `os' packages without a C compiler,
             ;; but on Guix a C compiler is necessary to properly link the
             ;; final binaries anyway.  Many build flags also invalidate
             ;; these pre-compiled archives, so in practice Go often
             ;; recompiles them anyway.
             ;;
             ;; Upstream is also planning to no longer install these
             ;; archives: <https://github.com/golang/go/issues/47257>
             ;;
             ;; When necessary, a custom pre-compiled library package can
             ;; be created with `#:import-path "std"' and used with
             ;; `-pkgdir'.
             (let* ((out (assoc-ref outputs "out"))
                    (tests (assoc-ref outputs "tests")))
               (for-each
                (lambda (file)
                  (copy-recursively file (string-append out "/lib/go/" file)))
                '("lib" "VERSION" "pkg/include" "pkg/tool"))

               (for-each
                (match-lambda
                  ((file dest output)
                   ;; Copy to output/dest and symlink from output/lib/go/file.
                   (let ((file* (string-append output "/lib/go/" file))
                         (dest* (string-append output "/" dest)))
                     (copy-recursively file dest*)
                     (mkdir-p (dirname file*))
                     (symlink (string-append "../../" dest) file*))))
                `(("bin"          "bin"                 ,out)
                  ("src"          "share/go/src"        ,out)
                  ("misc"         "share/go/misc"       ,out)
                  ("doc"          "share/doc/go/doc"    ,out)
                  ("api"          "share/go/api"        ,tests)
                  ("test"         "share/go/test"       ,tests))))))
         (add-after 'install 'install-doc-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (file)
                  (install-file file (string-append out "/share/doc/go")))
                '("AUTHORS" "CONTRIBUTORS" "CONTRIBUTING.md" "PATENTS"
                  "README.md" "SECURITY.md"))))))))
    (inputs (if (not (target-arm?))
                (alist-delete "gcc:lib"
                              (package-inputs go-1.17))
                (package-inputs go-1.17)))
    (native-inputs
     (if (not (member (%current-system)
                      (package-supported-systems go-1.4)))
         (alist-replace "go"
                        (list go-1.17)
                        (package-native-inputs go-1.17))
         (package-native-inputs go-1.17)))
    (home-page "https://go.dev")))

(define-public go-golang-org-x-sys
  (package
    (name "go-golang-org-x-sys")
    (version "0.0.0-20220610221304-9f5ed59c137d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/sys")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ffvdyv8fz8jwwn8s7pp8kk5cfzrkrfxwsp26zzj0xxhwjn668hi"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "golang.org/x/sys"
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


(define-public go-github-com-quasilyte-go-ruleguard-dsl
  (package
    (name "go-github-com-quasilyte-go-ruleguard-dsl")
    (version "0.3.21")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/quasilyte/go-ruleguard")
                    (commit (string-append "dsl/v" (go-version->git-ref version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qad7igj77dcnahqaqzsw9302iq0m1jfi2rj0zz2nwscvxzz41af"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/quasilyte/go-ruleguard/dsl"
       #:unpack-path "github.com/quasilyte/go-ruleguard"
       #:tests? #f))
    (home-page "https://github.com/quasilyte/go-ruleguard")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-go-ping-ping
  (package
    (name "go-github-com-go-ping-ping")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-ping/ping")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qqgdwaiv2793ikfnpzi9dvsilfvpxizsgnzf0wkb13d7jw6cf9q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-ping/ping"
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-google-uuid" ,go-github-com-google-uuid)))
    (home-page "https://github.com/go-ping/ping")
    (synopsis "go-ping")
    (description
     "Package ping is a simple but powerful ICMP echo (ping) library.")
    (license license:expat)))

(define-public go-github-com-jessevdk-go-flags
  (package
    (name "go-github-com-jessevdk-go-flags")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jessevdk/go-flags")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13ixw1yx4bvcj66lkc8zgwf9j7gkvj686g991gycdsafvdvca0lj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jessevdk/go-flags"
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/jessevdk/go-flags")
    (synopsis "go-flags: a go library for parsing command line arguments")
    (description
     "Package flags provides an extensive command line option parser.  The flags
package is similar in functionality to the go built-in flag package but provides
more options and uses reflection to provide a convenient and succinct way of
specifying command line options.")
    (license license:bsd-3)))
