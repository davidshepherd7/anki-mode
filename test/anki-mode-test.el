
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

(ert-deftest test-update-decks ()
  (with-mock
    (mock (anki-mode-connect 'anki-mode--update-decks-cb "deckNames" nil *))
    (anki-mode-update-decks))

  (let ((anki-mode-decks '()))
    (anki-mode--update-decks-cb '("foo" "bar"))
    (should (equal anki-mode-decks '("foo" "bar")))))


(ert-deftest test-create-card ()
  (with-mock
    (mock (anki-mode-connect 'anki-mode--create-card-cb "addNotes"
                             '((deckName . "foo deck")
                               (modelName . "bar Model")
                               (fields . ((front . "<p>what is <em>foo</em></p>")
                                          (back . "<p>everything</p>")))) *))
    (anki-mode-create-card "foo deck" "bar Model"
                           '((front . "what is *foo*")
                             (back . "everything")))))
