;; This spec builds the manifest from 'examples/random-manifest.scm', which is
;; possible because Cuirass itself is a channel.  This is a useful way to test
;; Cuirass itself and its build mechanism.

(list (specification
       (name 'random)
       (build '(manifests "examples/random-manifest.scm"))
       (build-outputs
        ;; Define a "build product" for some of the jobs.
        (list (build-output
               (job "(random|entropy-)1[0-9]")
               (type "archive")
               (output "out")
               (path ""))))
       (channels
        (cons (channel
               (name 'cuirass)
               (url (canonicalize-path
                     (string-append (dirname (current-filename))
                                    "/.."))))
              %default-channels))))
