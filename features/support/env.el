(require 'f)

(defvar anki-mode-support-path
  (f-dirname load-file-name))

(defvar anki-mode-features-path
  (f-parent anki-mode-support-path))

(defvar anki-mode-root-path
  (f-parent anki-mode-features-path))

(add-to-list 'load-path anki-mode-root-path)

(require 'anki-mode)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
