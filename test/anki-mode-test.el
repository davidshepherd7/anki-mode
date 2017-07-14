
(ert-deftest test-check-version ()
  ;; call
  (with-mock
    (mock (anki-mode-connect 'anki-mode--check-version-cb "version" nil *))
    (anki-mode-check-version))

  ;; callback
  (let ((anki-mode--required-anki-connect-version 2))
    (with-mock
      (mock (message *))
      (anki-mode--check-version-cb 1))
    (with-mock
      (not-called message)
      (anki-mode--check-version-cb 2))))
