;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

;; Load anki-mode
(require 'f)
(add-to-list 'load-path (f-parent (f-dirname load-file-name)))
(require 'anki-mode)

;; Load test helpers
(require 'el-mock)
(eval-when-compile
  (require 'cl)) ;; for el-mock
