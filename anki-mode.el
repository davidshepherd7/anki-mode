;;; anki-mode.el --- TODO -*- lexical-binding: t; -*-

;; Copyright © 2017 David Shepherd

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (dash "2.12.0") (markdown-mode "2.2") (s "1.11.0") (request "0.3.0"))
;; Keywords: TODO
;; URL: https://github.com/davidshepherd7/anki-mode


;;; Commentary:

;; TODO


;;; Code:

(require 'markdown-mode)
(require 'dash)
(require 's)
(require 'request)
(require 'json)

(eval-when-compile
  (require 'cl))



(defvar anki-mode--required-anki-connect-version 2
  "Version of the anki connect plugin required")

(defvar anki-mode-decks '()
  "List of anki deck names. Update with `'anki-mode-update-decks'")

(defvar anki-mode-card-type nil
  "Buffer local variable containing the current card type")


(defvar anki-mode--card-types '(("Basic" ("Front" "Back"))
                       ("Cloze" ())
                       ("Basic (and reversed card)" ("Front" "Back")))
  "TODO: get from anki")



(defcustom anki-mode-markdown-command (or markdown-command "markdown")
  "Markdown command to run to convert markdown to html. Attempts
to default to the one used by markdown mode if it is set."
  :group 'anki-mode)



(define-derived-mode anki-mode markdown-mode "Anki")

(define-key anki-mode-map (kbd "C-c C-c") #'anki-mode-send-and-new-card)
(define-key anki-mode-map (kbd "$") #'anki-mode-insert-latex-math)

(defun anki-mode-send-and-new-card ()
  (interactive)
  (anki-mode-send-new-card)
  (anki-mode-new-card))

(defun anki-mode-insert-latex-math ()
  (interactive)
  ;; TODO: handle regions
  (insert "[$][/$]")
  (forward-char -4))

;;;###autoload
(defun anki-mode-new-card ()
  (interactive)
  (let ((previous-card-type anki-mode-card-type))
    (find-file (make-temp-file "anki-card-"))
    (anki-mode)
    (setq-local anki-mode-card-type
                (or previous-card-type
                    (completing-read "Choose card type: "
                                     (-map #'car anki-mode--card-types))))

    (insert "@Front\n\n")
    (insert "@Back\n")
    (forward-line -2)))

(defun anki-mode-markdown (string)
  (interactive)
  (with-temp-buffer
    (insert string)
    (shell-command-on-region (point-min) (point-max) anki-mode-markdown-command (buffer-name))
    (s-trim (buffer-string))))

(defun anki-mode-connect (callback method params sync)
  (request "http://localhost:8765"
           :type "POST"
           :data (json-encode `(("action" . ,method) ("params" . ,params)))
           :headers '(("Content-Type" . "application/json"))
           :parser 'json-read
           :sync sync
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       (if (not data)
                           (message "Warning: anki-mode-connect got null data, this probably means a bad query was sent")
                         (funcall callback data))))))

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
  (setq anki-mode-decks decks))


(defun anki-mode-create-card (deck model fields)
  (let ((md-fields (-map (lambda (pair) (cons (car pair) (anki-mode-markdown (cdr pair)))) fields)))
    (anki-mode-connect #'anki-mode--create-card-cb "addNotes" `((deckName . ,deck)
                                              (modelName . ,model)
                                              (fields . ,md-fields)) t)))
(defun anki-mode--create-card-cb (ret)
  (message "Created card, got back %S" ret))


(defun anki-mode-send-new-card ()
  (interactive)
  (let ((default-directory "~/code/ankicli")
        (is-cloze (not (equal 0 (anki-mode--max-cloze)))))
    (when (not (equal 0
                      (apply #'call-process
                             "~/code/ankicli/bin/anki-cli" (buffer-file-name) t t
                             (-concat '("--markdown")
                                      (if is-cloze
                                          '("--model" "Cloze")
                                        '("--model" "Basic (and reversed card)"))
                                      ))))
      (error "anik-cli failed"))))


(defun anki-mode--max-cloze ()
  (--> (buffer-substring-no-properties (point-min) (point-max))
       (s-match-strings-all "{{c\\([0-9]\\)::" it)
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
