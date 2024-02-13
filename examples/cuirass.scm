;; This spec builds Cuirass itself, which is possible because Cuirass itself
;; is a channel, with its '.guix-channel' file.

(list (specification
       (name 'cuirass)
       (build '(channels . (cuirass)))
       (channels
        (cons (channel
               (name 'cuirass)
               (url "https://git.savannah.gnu.org/git/guix/guix-cuirass.git")
               (branch "main"))
              %default-channels))))
