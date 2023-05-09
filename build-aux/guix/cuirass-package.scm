;;;; guix.scm -- Guix package definition
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cuirass-package)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix search-paths)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls))

(define source-checkout
  (let ((vcs-file? (or (git-predicate
                        (string-append (current-source-directory)
                                       "/../.."))
                       (const #t))))
    (local-file "../.." "cuirass-checkout"
                #:recursive? #t
                #:select? vcs-file?)))

(define %cuirass-version "1.99.99-git")

(define-public cuirass
  (package
    (name "cuirass")
    (version %cuirass-version)
    (source source-checkout)
    (build-system gnu-build-system)
    (arguments
     (list #:modules '((guix build utils)
                       (guix build gnu-build-system)
                       (ice-9 rdelim)
                       (ice-9 popen))
           #:configure-flags #~'("--localstatedir=/var") ;for /var/log/cuirass
           #:parallel-tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'fix-version-gen
                 (lambda _
                   (patch-shebang "build-aux/git-version-gen")

                   (call-with-output-file ".tarball-version"
                     (lambda (port)
                       (display #$(package-version this-package) port)))))
               (add-before 'check 'set-PATH-for-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((pg (assoc-ref inputs "ephemeralpg"))
                         (path (getenv "PATH")))
                     (setenv "PATH" (string-append pg "/bin:" path)))))
               ;; Disable the remote tests that require a Guix daemon connection.
               (add-before 'check 'disable-remote-tests
                 (lambda _
                   (substitute* "Makefile.am"
                     (("tests/remote.scm") ""))))
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Wrap the 'cuirass' command to refer to the right modules.
                   (let* ((out    (assoc-ref outputs "out"))
                          (avahi  (assoc-ref inputs "guile-avahi"))
                          (gcrypt (assoc-ref inputs "guile-gcrypt"))
                          (json   (assoc-ref inputs "guile-json"))
                          (zmq    (assoc-ref inputs "guile-simple-zmq"))
                          (squee  (assoc-ref inputs "guile-squee"))
                          (git    (assoc-ref inputs "guile-git"))
                          (bytes  (assoc-ref inputs "guile-bytestructures"))
                          (fibers (assoc-ref inputs "guile-fibers"))
                          (zlib   (assoc-ref inputs "guile-zlib"))
                          (matd   (assoc-ref inputs "guile-mastodon"))
                          (tls    (assoc-ref inputs "guile-gnutls"))
                          (mail   (assoc-ref inputs "mailutils"))
                          (guix   (assoc-ref inputs "guix"))
                          (deps   (list avahi gcrypt json zmq squee git bytes
                                        fibers zlib matd tls mail guix))
                          (guile  (assoc-ref inputs "guile"))
                          (effective
                           (read-line
                            (open-pipe* OPEN_READ
                                        (string-append guile "/bin/guile")
                                        "-c" "(display (effective-version))")))
                          (mods
                           (string-drop-right     ;drop trailing colon
                            (string-join deps
                                         (string-append "/share/guile/site/"
                                                        effective ":")
                                         'suffix)
                            1))
                          (objs
                           (string-drop-right
                            (string-join deps
                                         (string-append "/lib/guile/" effective
                                                        "/site-ccache:")
                                         'suffix)
                            1)))
                     ;; Make sure 'cuirass' can find the relevant Guile modules.
                     (wrap-program (string-append out "/bin/cuirass")
                       `("PATH" ":" prefix (,(string-append out "/bin")))
                       `("GUILE_LOAD_PATH" ":" prefix (,mods))
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,objs)))))))))
    (inputs
     (list guile-3.0-latest
           guile-avahi
           guile-fibers-1.1
           guile-gcrypt
           guile-json-4
           guile-simple-zmq
           guile-squee
           guile-git
           guile-zlib
           guile-mastodon
           guile-gnutls
           mailutils
           ;; FIXME: this is propagated by "guile-git", but it needs to be among
           ;; the inputs to add it to GUILE_LOAD_PATH.
           guile-bytestructures

           guix))
    (native-inputs
     (list autoconf automake pkg-config texinfo ephemeralpg))
    (native-search-paths
     ;; For HTTPS access, Cuirass itself honors these variables, with the
     ;; same semantics as Git and OpenSSL (respectively).
     (list (search-path-specification
            (variable "GIT_SSL_CAINFO")
            (file-type 'regular)
            (separator #f)                        ;single entry
            (files '("etc/ssl/certs/ca-certificates.crt")))
           $SSL_CERT_DIR))
    (synopsis "Continuous integration system")
    (description
     "Cuirass is a continuous integration tool using GNU Guix.  It is
intended as a replacement for Hydra.")
    (home-page "https://guix.gnu.org/cuirass/")
    (license license:gpl3+)))

;; Return the Cuirass package that lets you build from Git, for the benefit
;; of 'guix shell'.
cuirass
