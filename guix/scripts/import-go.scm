;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix scripts import-go)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import-go)
  #:use-module (guix import utils)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:export (guix-import-go))


;;;
;;; Helper.
;;;

(define (newline-rewriting-port output)
  "Return an output port that rewrites strings containing the \\n escape
to an actual newline.  This works around the behavior of `pretty-print'
and `write', which output these as \\n instead of actual newlines,
whereas we want the `description' field to contain actual newlines
rather than \\n."
  (define (write-string str)
    (let loop ((chars (string->list str)))
      (match chars
        (()
         #t)
        ((#\\ #\n rest ...)
         (newline output)
         (loop rest))
        ((chr rest ...)
         (write-char chr output)
         (loop rest)))))

  (make-soft-port (vector (cut write-char <>)
                          write-string
                          (lambda _ #t)           ; flush
                          #f
                          (lambda _ #t)           ; close
                          #f)
                  "w"))


;;;
;;; Command-line options.
;;;

(define %default-options
  '((goproxy . "https://proxy.golang.org")))

(define (show-help)
  (display (G_ "Usage: guix import go PACKAGE-PATH[@VERSION]
Import and convert the Go module for PACKAGE-PATH.  Optionally, a version
can be specified after the arobas (@) character.\n"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        generate package expressions for all Go modules
that are not yet in Guix"))
  (display (G_ "
  -p, --goproxy=GOPROXY  specify which goproxy server to use"))
  (display (G_ "
  --pin-versions         use the exact versions of a module's dependencies"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         (option '(#\p "goproxy") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'goproxy arg (alist-delete 'goproxy result))))
         (option '("pin-versions") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'pin-versions? #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;


(define (import . args)
    (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (with-error-handling
    (let* ((opts (parse-options))
           (args (filter-map (match-lambda
                               (('argument . value)
                                value)
                               (_ #f))
                             (reverse opts)))
           ;; Append the full version to the package symbol name when using
           ;; pinned versions.
           (package->definition* (if (assoc-ref opts 'pin-versions?)
                                     (cut package->definition <> 'full)
                                     package->definition)))
      (match args
        ((spec)               ;e.g., github.com/golang/protobuf@v1.3.1
         (receive (name version)
             (package-name->name+version spec)
           (let ((arguments (list name
                                  #:goproxy (assoc-ref opts 'goproxy)
                                  #:version version
                                  #:pin-versions?
                                  (assoc-ref opts 'pin-versions?))))
             (if (assoc-ref opts 'recursive)
                 ;; Recursive import.
                 (map package->definition*
                      (apply go-module-recursive-import arguments))
                 ;; Single import.
                 (let ((sexp (apply go-module->guix-package* arguments)))
                   (unless sexp
                     (leave (G_ "failed to download meta-data for module '~a'.~%")
                            name))
                   (package->definition* sexp))))))
        (()
         (leave (G_ "too few arguments~%")))
        ((many ...)
         (leave (G_ "too many arguments~%")))))))

(define-command (guix-import-go . args)
  (category packaging)
  (synopsis "import a package definition from an external repository")

  (let ((print (lambda (expr)
                 (pretty-print expr (newline-rewriting-port
                                     (current-output-port))
                               #:max-expr-width 80))))
    (match (apply import args)
      ((and expr (or ('package _ ...)
                     ('let _ ...)
                     ('define-public _ ...)))
       (print expr))
      ((? list? expressions)
       (for-each (lambda (expr)
                   (print expr)
                   (newline))
                 expressions))
      (x
       (leave (G_ "import failed~%"))))))
