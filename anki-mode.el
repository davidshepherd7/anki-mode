;;; anki-mode.el --- TODO -*- lexical-binding: t; -*-

;; Copyright © 2017 David Shepherd

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (dash "2.12.0") (markdown-mode "2.2") (s "1.11.0") (request "0.3.0"))
;; Keywords: TODO
;; URL: https://github.com/davidshepherd7/anki-mode


;;; Commentary:

;; TODO: documentation

;; TODO: get card types from anki

;; TODO: list/edit existing cards?

;; TODO: look at cloze stuff again

;; TODO: testing with hash tables?


;;; Code:

(require 'markdown-mode)
(require 'dash)
(require 's)
(require 'request)
(require 'json)

(eval-when-compile
  (require 'cl))

;; (setq request-log-level 'debug)
;; (setq request-message-level 'debug)



(defvar anki-mode--required-anki-connect-version 5
  "Version of the anki connect plugin required")

(defvar anki-mode-decks '()
  "List of anki deck names. Update with `'anki-mode-update-decks'")

(defvar anki-mode--card-types '(("Basic" . ("Front" "Back"))
                            ("Cloze" . ("Text" "Extra"))
                            ("Basic (and reversed card)" . ("Front" "Back")))
  "TODO: get from anki")


(defvar anki-mode-deck nil
  "Buffer local variable containing the current deck")

(defvar anki-mode-card-type nil
  "Buffer local variable containing the current card type")




(defcustom anki-mode-markdown-command "pandoc --from markdown_github --to html"
  "Markdown command to run to convert markdown to html.

Use pandoc by default because it can do sensible things with underscores in LaTeX."
  :group 'anki-mode
  :type 'string)



;;; Interface

;; github flavoured markdown mode instead of markdown mode because it works with
;; underscores, which come up a lot in LaTeX maths and when talking about code.
(define-derived-mode anki-mode gfm-mode "Anki")

(define-key anki-mode-map (kbd "C-c C-c") #'anki-mode-send-new-card)
(define-key anki-mode-map (kbd "$") #'anki-mode-insert-latex-math)

(defun anki-mode-insert-latex-math ()
  (interactive)
  (if (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (insert "[$]")
        (goto-char (region-end))
        (insert "[/$]"))
    ;; else
    (insert "[$][/$]")
    (forward-char -4)))

;;;###autoload
(defun anki-mode-new-card ()
  (interactive)

  (unless anki-mode-decks (anki-mode-refresh))

  (let ((deck (completing-read "Choose deck: " anki-mode-decks))
        (card-type (completing-read "Choose card type: "
                                    (-map #'car anki-mode--card-types))))

    (find-file (make-temp-file "anki-card-"))
    (anki-mode)
    (setq-local anki-mode-deck deck)
    (setq-local anki-mode-card-type card-type)

    (let ((card-fields (assoc anki-mode-card-type anki-mode--card-types)))
      (unless card-fields
        (error "Unrecognised card type: \"%s\"" anki-mode-card-type))
      (-each (cdr card-fields)
        (lambda (field)
          (insert (s-concat "@" field "\n\n\n")))))

    (goto-char (point-min))
    (forward-line 1)))



;;; Menu page

(define-derived-mode anki-mode-menu-mode special-mode "Anki Menu"
  "Major mode for the anki-mode menu page."
  (let ((inhibit-read-only t))
    (insert "Anki Mode\n")
    (insert "---------------\n")
    (insert "[n]: New card\n")
    (insert "[r]: Refresh anki decks list\n")))

(define-key anki-mode-menu-mode-map (kbd "n") #'anki-mode-new-card)
(define-key anki-mode-menu-mode-map (kbd "r") #'anki-mode-refresh)

(defun anki-mode-menu-buffer ()
  (or (get-buffer "*Anki*")
      (with-current-buffer (get-buffer-create "*Anki*")
        (anki-mode-menu-mode)
        (current-buffer))))

(defun anki-mode-menu ()
  (interactive)
  (switch-to-buffer (anki-mode-menu-buffer)))



;;; Anki-connect helpers

(defun anki-mode-connect (callback method params sync)
  (let ((data (--> (list (cons "action" method)
                         (cons "version" anki-mode--required-anki-connect-version))
                   (-concat it (if params (list (cons "params" params)) (list)))
                   (json-encode it)
                   (encode-coding-string it 'utf-8))))
    (message "Anki connect sending %S" data)
    (request "http://localhost:8765"
             :type "POST"
             :data data
             :headers '(("Content-Type" . "application/json"))
             :parser 'json-read
             :sync sync
             :success (anki-mode--http-success-factory callback)
             :error  (function* (lambda (&key error-thrown &allow-other-keys)
                                  (error "Got error: %S" error-thrown))))))

(defun anki-mode--http-success-factory (callback)
  (function*
   (lambda (&key data &allow-other-keys)
     (when (not data)
       (message "Warning: anki-mode-connect got null data, this probably means a bad query was sent"))

     ;; (message "Got anki data %S" data)
     (let ((the-error (cdr (assoc 'error data)))
           (the-result (cdr (assoc 'result data))))
       (when the-error
         (error "Anki connect returned error: %S" the-error))
       (funcall callback the-result)))))

(defun anki-mode-refresh ()
  (interactive)
  (anki-mode-check-version)
  (anki-mode-update-decks)
  ;; TODO: card types
  )

(defun anki-mode-check-version ()
  (interactive)
  (anki-mode-connect #'anki-mode--check-version-cb "version" nil t))
(defun anki-mode--check-version-cb (version)
  (when (not (= version anki-mode--required-anki-connect-version))
    (message "Warning you have anik connect version %S installed, but %S is required"
             version anki-mode--required-anki-connect-version)))

(defun anki-mode-update-decks ()
  (interactive)
  (anki-mode-connect #'anki-mode--update-decks-cb "deckNames" nil t))
(defun anki-mode--update-decks-cb (decks)
  ;; Convert vector to list
  (setq anki-mode-decks (append decks nil)))




;;; Card creation

(defun anki-mode--markdown (string)
  (interactive)
  (with-temp-buffer
    (insert string)
    (shell-command-on-region (point-min) (point-max) anki-mode-markdown-command (buffer-name))
    (s-trim (buffer-string))))

(defun anki-mode-create-card (deck model fields)
  (save-buffer)
  (let ((md-fields (-map (lambda (pair) (cons (car pair) (anki-mode--markdown (cdr pair)))) fields))
        ;; Unfortunately emacs lisp doesn't distinguish between {"notes" :
        ;; [...]} and ["notes", ...] in alists or plists, so we have to use a
        ;; hash table for this.
        (hash-table (make-hash-table)))
    (puthash 'notes `(((deckName . ,deck)
                       (modelName . ,model)
                       (fields . ,md-fields))) hash-table)
    (anki-mode-connect #'anki-mode--create-card-cb "addNotes" hash-table t)))
(defun anki-mode--create-card-cb (ret)
  (message "Created card, got back %S" ret)
  (anki-mode-menu))

(defun anki-mode--parse-fields (string)
  (--> string
       (s-split "^\\s-*@" it)
       (-map #'s-trim it)
       (-filter (lambda (field) (not (s-blank? field))) it)
       (-map (lambda (field) (s-split-up-to "\n" field 1)) it)
       (-map #'anki-mode--list-to-pair it)))

(defun anki-mode--list-to-pair (li)
  (cons (car li) (cadr li)))

;;;###autoload
(defun anki-mode-send-new-card ()
  (interactive)
  (anki-mode-create-card anki-mode-deck anki-mode-card-type (anki-mode--parse-fields (buffer-substring-no-properties (point-min) (point-max)))))



;;; Cloze helpers

(defun anki-mode--max-cloze ()
  (--> (buffer-substring-no-properties (point-min) (point-max))
       (s-match-strings-all "{{c\\([0-9]+?\\)::" it)
       (-map #'cadr it) ; First group of each match
       (-map #'string-to-number it)
       (or it '(0))
       (-max it)))

(defun anki-mode-cloze-region (start end)
  (interactive "r")
  (save-excursion
    ;; Do end first because inserting at start moves end
    (goto-char end)
    (insert "}}")

    (goto-char start)
    (insert "{{c")
    (insert (number-to-string (1+ (anki-mode--max-cloze))))
    (insert "::")
    ))



(provide 'anki-mode)


;;; anki-mode.el ends here
