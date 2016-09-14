;;; guix.scm -- Guix package definition
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (gnu packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages databases)
             (gnu packages guile)
             (gnu packages package-management)
             (gnu packages pkg-config)
             (guix git-download)
             (guix licenses)
             (guix packages)
             (guix build-system gnu))

(define-public cuirass
  (package
    (name "cuirass")
    (version "0.0.ff7c3a1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://notabug.org/mthl/cuirass")
                    (commit "master")))
              (sha256
               (base32
                "1jw3smw6axqr58ahkyjncygv0nk3hdrqkv0hm4awwj0hg5nl3d2p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'bootstrap
            (lambda _ (zero? (system* "sh" "bootstrap")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("guile" ,guile-2.0)
       ("guile-json" ,guile-json)
       ("guile-sqlite3" ,guile-sqlite3)
       ("guix" ,guix)
       ("pkg-config" ,pkg-config)
       ("sqlite" ,sqlite)))
    (synopsis "Continuous integration system")
    (description
     "Cuirass is a continuous integration system which uses GNU Guix.  It is
intended as replacement for Hydra.")
    (home-page "https://notabug.org/mthl/cuirass")
    (license gpl3+)))

;; Return it here so 'guix build/environment/package' can consume it directly.
cuirass
