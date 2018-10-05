
(ert-deftest test-request-callback-success ()
  (let* ((mock-callback (lambda (result)
                          (should (s-equals? result "some data"))))
         (function-to-test (anki-mode--http-success-factory mock-callback)))
    (funcall function-to-test
             :data '((result . "some data") (error))
             :foo "other ignored thing")))


(ert-deftest test-request-callback-error ()
  (let* ((mock-callback (lambda (result)
                          ;; should never be called
                          (should nil)))
         (function-to-test (anki-mode--http-success-factory mock-callback)))
    (should-error (funcall function-to-test
                           :data '((result) (error "uh oh"))
                           :foo "other ignored thing"))))



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
    ;; request seems to give us a vector, not a list
    (anki-mode--update-decks-cb '["foo" "bar"])
    (should (equal anki-mode-decks '("foo" "bar")))))


(ert-deftest test-create-card ()
  ;; See impl for why a hash table
  (let ((expected (make-hash-table)))
    (puthash 'notes '(((deckName . "foo deck")
                       (modelName . "bar Model")
                       (fields . (("front" . "<p>what is <em>foo</em></p>")
                                  ("back" . "<p>everything</p>"))))) expected)
    (with-mock
      ;; TODO: we can't use "expected" here because el-mock doesn't know how to
      ;; compare hash tables (and neither do I...)
      (mock (anki-mode-connect 'anki-mode--create-card-cb "addNotes" * *))
      (anki-mode-create-card "foo deck" "bar Model"
                             '(("front" . "what is *foo*")
                               ("back" . "everything"))))))


(ert-deftest test-send-buffer-as-new-card ()
  (with-temp-buffer
    (setq-local anki-mode-deck "foo deck")
    (setq-local anki-mode-card-type "bar model")

    (let ((anki-mode-card-types '("bar model" ("front" "back"))))

      (insert "@front\n")
      (insert "some text content\n")
      (insert "@back\n")
      (insert "some more buffer text\n")

      (with-mock
        (mock (anki-mode-create-card "foo deck" "bar model"
                                     '(("front" . "some text content")
                                       ("back" . "some more buffer text"))))

        (anki-mode-send-new-card)))))


(ert-deftest test-send-buffer-as-new-card-blank-field ()
  (with-temp-buffer
    (setq-local anki-mode-deck "foo deck")
    (setq-local anki-mode-card-type "bar model")

    (let ((anki-mode-card-types '("bar model" ("front" "back"))))

      (insert "@front\n")
      (insert "some text content\n")
      (insert "@back\n")

      (with-mock
        (mock (anki-mode-create-card "foo deck" "bar model"
                                     '(("front" . "some text content")
                                       ("back" . ""))))

        (anki-mode-send-new-card)))))



(ert-deftest test-new-card ()
  (let ((anki-mode-decks '("Default" "MOCK read"))
        (anki-mode--card-types '(("Basic" . ("Front" "Back"))
                                 ("Cloze" . ())
                                 ("Basic (and reversed card)" . ("Front" "Back"))
                                 ("MOCK read" . ("foo" "bar")))))

    ;; current card/deck is used
    (with-temp-buffer
      (insert "old card")

      (anki-mode-new-card-noninteractive "Default" "Basic")

      (should (s-matches? "anki-card-.*" (buffer-name)))
      (should (derived-mode-p 'anki-mode))

      (should (s-equals? anki-mode-deck "Default"))
      (should (s-equals? anki-mode-card-type "Basic"))

      (should (s-matches? "^@Front" (buffer-string)))
      (should (s-matches? "^@Back" (buffer-string))))


    ;; no current card/deck
    (with-mock
      (mock (completing-read * *) => "MOCK read" :times 2)

      (anki-mode-new-card)
      (should (s-matches? "anki-card-.*" (buffer-name)))
      (should (derived-mode-p 'anki-mode))

      (should (s-equals? anki-mode-previous-deck "MOCK read"))
      (should (s-equals? anki-mode-previous-card-type "MOCK read"))

      (should (s-matches? "^@foo" (buffer-string)))
      (should (s-matches? "^@bar" (buffer-string))))
    ))


(ert-deftest test-latex-math-insertion ()

  ;; Default
  (with-temp-buffer
    (call-interactively #'anki-mode-insert-latex-math)
    (should (s-equals? (buffer-string) "[$][/$]"))
    (should (looking-at-p (regexp-quote "[/$]"))))

  ;; Region
  (with-temp-buffer
    (transient-mark-mode)

    (insert "foo\n")
    (insert "\\alpha = \\beta ")
    (insert "bar")

    (goto-char 5)
    (set-mark 19)

    (call-interactively #'anki-mode-insert-latex-math)

    ;; region has been wrapped with latex math
    (should (s-contains? "[$]\\alpha = \\beta[/$]" (buffer-string)))
    ;; Point is not moved
    (should (looking-at-p (regexp-quote "[$]")))
    ))

(ert-deftest test-next-field-no-markers ()
  ;; No @ symbols
  (with-temp-buffer
    (should-error (anki-mode-next-field))))

(ert-deftest test-next-field-typical-buffer ()
  ;; Typical buffer
  (with-temp-buffer
    (insert "@front\n")
    (insert "foo\n")
    (insert "@back\n")
    (insert "bar\n")
    (goto-char (point-min))

    (anki-mode-next-field)
    (should (looking-at-p "foo"))

    (anki-mode-next-field)
    (should (looking-at-p "bar"))

    (anki-mode-next-field)
    (should (looking-at-p "foo"))))

(ert-deftest test-next-field-missing-final-newline ()
  ;; No trailing newline
  (with-temp-buffer
    (insert "@front\n")
    (insert "foo\n")
    (insert "@back")
    (goto-char (point-min))
    (anki-mode-next-field)
    (anki-mode-next-field)
    (should (looking-back "@back"))))
