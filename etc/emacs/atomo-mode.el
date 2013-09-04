;;; @(#) atomo-mode.el --- Atomo mode

;; Copyright (C) 2011 Brian T. Rice.

;; Authors: Brian T. Rice <BrianTRice@gmail.com>
;; Created: July 31, 2011
;; Keywords: languages oop

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Some recognition of method definition syntax is still needed in order to
;; recognize them, which would enable intelligent highlighting of the signature
;; (which is lacking) as well as indentation. Specifically, avoiding the
;; highlighting of `atomo-globals-regexp' keywords while being able to
;; make the selector (unary, binary, not just keyword) stands out correctly
;; is desired.

;;; Code:

(defvar atomo-mode-abbrev-table nil
  "Abbrev table in use in atomo-mode buffers.")
(define-abbrev-table 'atomo-mode-abbrev-table ())

;; Customize Support
;; =================

(defgroup atomo nil
  "Major mode for editing Atomo code."
  :prefix "atomo-"
  :group 'languages)

(defcustom atomo-mode-hook nil
  "Normal hook run when entering Atomo mode."
  :type 'hook
  :options '(imenu-add-menubar-index)
  :group 'atomo)

(defcustom atomo-indent-increment 2
  "Amount to adjust indentation by."
  :type 'integer
  :options '(2 4)
  :group 'atomo)

(defvar atomo-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar
     #'(lambda (l)
         (define-key map (car l) (cdr l)))
     `(("\M-\t" . atomo-tab)
       ("\t" . atomo-reindent)
       ([backspace] . backward-delete-char-untabify)
       ("\n" . atomo-newline-and-indent)
       ("\M-\C-a" . atomo-begin-of-defun)
       ("\M-\C-f" . atomo-forward-sexp)
       ("\M-\C-b" . atomo-backward-sexp)
       (":" . atomo-colon)
       ;;("@" . atomo-dispatch)
       ;;("\C-c\C-d" . atomo-category-definition)
       ;;("\C-cc" . atomo-compile)
       ("\C-cd" . atomo-macroexpand-region)
       ("\C-ce" . atomo-eval-region)
       ("\C-cf" . atomo-filein)
       ("\C-cm" . atomo)
       ("\C-cp" . atomo-print)
       ("\C-cq" . atomo-quit)
       ;;("\C-cr" . atomo-reeval-region)
       ("\C-cs" . atomo-snapshot)
       ;;("\C-ct" . ,atomo-template-map)
       ("\C-cu" . atomo-unit-tests)
       ("\C-cw" . atomo-workspace)
       ;;("\C-c\C-s" . atomo-browse-selectors)
       ;;("\C-x c" . atomo-complete-traits)
       ))
    map)
  "Atomo mode keymappings")

;; Syntax-Handling
;; ===============

(defconst atomo-name-chars "A-Za-z0-9"
  "The collection of character that can compose a Atomo identifier")

(defconst atomo-name-regexp (concat "[A-Za-z][-" atomo-name-chars "_:]*[^:]")
  "A regular expression that matches a Atomo identifier")

(defconst atomo-globals-regexp
  (regexp-opt '("lobby" "True" "False" "Nil" "NoRole" "thisContext"
        "resend" "clone" "here" "it" "_") 'words))

(defconst atomo-binop-chars "-+*/\\~;<>=&?"
  "The collection of characters that can compose a Atomo binary selector.")

(defconst atomo-binop-regexp
  (concat "\\([" atomo-binop-chars "]+\\|[" atomo-binop-chars "]+[" atomo-name-chars "]*[" atomo-binop-chars "]+\\)")
  "A regular expression that matches a Atomo binary selector")

(defconst atomo-keyword-regexp
  (concat "\\([-" atomo-name-chars "_]+[-" atomo-name-chars "_:]*:\\| :[^A-Za-z]+\\)")
  "A regular expression that matches a Atomo keyword")

(defconst atomo-opt-keyword-regexp (concat "[&]\\(" atomo-keyword-regexp "\\|" atomo-name-regexp "\\)")
  "A regular expression that matches a Atomo optional-keyword")

(defconst atomo-whitespace-chars " \t\n\f")

(defconst hexdigit-regexp "[0-9a-fA-F]")

(defconst atomo-mode-syntax-table
  (let ((table (make-syntax-table)))
    (mapcar
     #'(lambda (l)
         (modify-syntax-entry (car l) (cdr l) table))
     '((?\' . "\"") ; String
       (?\" . "\"") ; Comment
       (?+  . "w") ; Binary selector elements...
       (?-  . "w")
       (?*  . "w")
       (?/  . "w")
       (?=  . "w")
       (??  . "w")
       (?%  . "w")
       (?~  . "w")
       (?%  . "w")
       (?\; . "w")
       (?<  . "w")
       (?>  . "w")
       (?\[ . "(]") ; Block opener
       (?\] . ")[") ; Block closer
       (?\( . "()") ; Parens opener
       (?\) . ")(") ; Parens closer
       (?{  . "(}") ; Array opener
       (?}  . "){") ; Array closer
       (?&  . "'") ; Optional keyword marker
       (?`  . "'") ; Macro character
       (?$  . "'") ; Character literal
       (?#  . "'") ; Symbol
       (?|  . "$") ; Locals
       (?_  . "w") ; Word-element and anonymous argument
       (?:  . "_") ; Keyword marker
       (?\\ . "\\") ; C-like escape
       (?!  . "'") ; A stop in Smalltalk. A type annotation in Atomo.
       (?@  . "'") ; Dispatch annotator
       (?^  . "w") ; Return
       (?,  . ".") ; Comma for *rest parameters
       (?.  . "."))) ; Statement separator
    table)
  "Atomo character types")

(defconst bold 'bold
  "Emacs requires this; ugly.")

(defconst italic 'italic)

(defconst atomo-font-lock-keywords
  `((,(concat "#[" atomo-name-chars atomo-binop-chars "_:]+")
     . font-lock-constant-face)                        ; symbol
    ("#'\\([^']\\|\\'\\)*'" . font-lock-constant-face) ; quoted symbol
    ("\"\\([^\"]\\|\\\"\\)\"" . font-lock-comment-face) ; comment
    (,(concat "[$]\\([^\\\\]\\|\\\\[^x]\\|\\\\x" hexdigit-regexp hexdigit-regexp "[^\\]\\)")
     . font-lock-string-face)                          ; character
    ("[^#$\\]'\\(.\\|\'\\)*'" . font-lock-string-face) ; string
    (,(concat "`" atomo-name-regexp ":[^" atomo-name-chars "_]")
     . ,(if (boundp 'font-lock-preprocessor-face)
        'font-lock-preprocessor-face
      'font-lock-builtin-face))         ; macro keyword call
    (,(concat "`\\(" atomo-binop-regexp "\\|" atomo-name-regexp "[:]?\\)\\>")
     . ,(if (boundp 'font-lock-preprocessor-face)
        'font-lock-preprocessor-face
      'font-lock-builtin-face))         ; macro call
    ("[^#]\\(=?:=\\>\\)"
     . ,(if (boundp 'font-lock-preprocessor-face)
        'font-lock-preprocessor-face
      'font-lock-keyword-face))      ; assignment/match/unify specials
    (,(concat "\\<[-+*/\\;&?]=\\>")
     . ,(if (boundp 'font-lock-preprocessor-face)
        'font-lock-preprocessor-face
      'font-lock-keyword-face))         ; op-assignment specials
    ("\\<\\(\\^[0-9^]?\\>\\)"
     . ,(if (boundp 'font-lock-preprocessor-face)
        'font-lock-preprocessor-face
     'font-lock-keyword-face))          ; return specials
    ("`+"
     . ,(if (boundp 'font-lock-preprocessor-face)
        'font-lock-preprocessor-face
      'font-lock-builtin-face))         ; quotation syntax
    (,atomo-opt-keyword-regexp
     . font-lock-variable-name-face)    ; optional keywords
    ("\\(?:_\\|[A-Za-z]+[_A-Za-z0-9]*\\)@+?"
     . font-lock-variable-name-face)    ; declaration dispatchings
    ("|[A-Za-z0-9:&_*!() \n]*|"
     . font-lock-variable-name-face)    ; block local slots
    ("\\<\\(:\\|&\\|*\\)[A-Za-z0-9_]+"
     . font-lock-variable-name-face)    ; block input slots
    (,atomo-keyword-regexp
     . font-lock-keyword-face)          ; keyword sends
    ("!\\([A-Za-z]*\\|\([A-Za-z0-9_ ]*\)\\)"
     . font-lock-type-face)             ; type-declaration
    ("\\<[+-]?[0-9]+\\([.][0-9]+\\)?\\>"
     . font-lock-constant-face)        ; numbers (integers and floats)
    (,(concat "\\<[+-]?[0-9_]+[Rr]" hexdigit-regexp "+\\([.]" hexdigit-regexp "+\\)?\\>")
     . font-lock-constant-face)        ; numbers (integers and floats)
    ("\\([.]\\)\\(?:$\\|[^0-9\"]\\)"
     . font-lock-warning-face)          ; statement separators
    ("\\(?:[A-Za-z0-9_]* \\)*\\(?:traits\\|derive\\)"
     . font-lock-type-face)                     ; traits name
    ("\\<[0-9_]+\\>" . font-lock-constant-face) ; integers
    (,atomo-globals-regexp
     . font-lock-builtin-face)          ; globals
;;     (,(concat "\\<" atomo-binop-regexp "\\>")
;;      . font-lock-string-face)        ; binary message send
   )
  "Atomo highlighting matchers.")

;; Inferior-Mode Support
;; =====================

(require 'comint)

(defconst atomo-prompt-regexp "^atomo[-A-z]*\[[0-9]+\]>"
  "Regexp to match prompts in atomo buffer.")

(defconst atomo-debug-prompt-regexp "^atomo-debug\[[0-9]+[.]?[.]?[0-9]+?\]>"
  "Regexp to match prompts in atomo buffer.")

(defconst atomo-prompt-line-regexp (concat atomo-prompt-regexp " .*")
  "Regexp to match the prompt line in the atomo buffer.")

(defconst atomo-debug-frame-regexp "^\\(frame: [0-9]+\\)"
  "Regexp to match the frame line label in the Atomo debugger.")

(defconst atomo-debug-fileref-regexp " @ \\([-A-z/_.]+:[0-9]+\\)$"
  "Regexp to match filename:linenumber in the Atomo debugger.")

(defconst atomo-debug-restart-regexp "^restart: [0-9]+"
  "Regexp to match restart listings in the Atomo debugger.")

(defvar atomo-cmd "atomo"
  "The name/path of the VM to be executed for the interactive mode.")

(defvar atomo-dir "."
  "The current working directory for the Atomo process; this should also be
   set in a preference. It should generally be one's atomo installation root.")

(defvar atomo-args '()
  "Arguments to pass to the `atomo-cmd' launcher. This should be overridden
   in the user's init file.")

(defvar *atomo-process* nil
  "The Atomo process")

(defvar inferior-atomo-buffer-name "*atomo*"
  "The Atomo interaction buffer name.")

(defvar atomo-output-buffer nil
  "Stores accumulating output from the Atomo printer.")

(defvar atomo-input-queue nil
  "Stores pending inputs to the Atomo reader.")

(defconst atomo-inf-mode-map (copy-keymap comint-mode-map)
  "The modemap used for interactive Atomo sessions.")
(set-keymap-parent atomo-inf-mode-map atomo-mode-map)

(defvar atomo-fileref-keymap (copy-keymap atomo-inf-mode-map))
(set-keymap-parent atomo-fileref-keymap atomo-inf-mode-map)
(define-key atomo-fileref-keymap [return] 'atomo-follow-name-at-point)
(define-key atomo-fileref-keymap [mouse-1] 'atomo-follow-name-at-point)

(defvar atomo-frameref-keymap (copy-keymap atomo-inf-mode-map))
(set-keymap-parent atomo-frameref-keymap atomo-inf-mode-map)
(define-key atomo-frameref-keymap [return] 'atomo-run-overlay-at-point)
(define-key atomo-frameref-keymap [mouse-1] 'atomo-run-overlay-at-point)

(defvar atomo-restart-keymap (copy-keymap atomo-inf-mode-map))
(set-keymap-parent atomo-restart-keymap atomo-inf-mode-map)
(define-key atomo-restart-keymap [return] 'atomo-run-overlay-at-point)
(define-key atomo-restart-keymap [mouse-1] 'atomo-run-overlay-at-point)

(defvar mode-status)

(defconst atomo-inf-font-lock-keywords
  `((,atomo-prompt-regexp . 'comint-highlight-prompt)    ; normal prompt
    (,atomo-debug-prompt-regexp . 'comint-highlight-prompt) ; debug prompt
    ("^\\(Warning\\|Error\\):" . 'font-lock-warning-face) ; warnings/errors
    ("^[ ]*\\(Loading\\) " . 'font-lock-warning-face) ; informative
    ;(,atomo-debug-fileref-regexp 1 'link) ; filename/lineno debugger reports
    ("^Atomo:" . compilation-info-face) ; VM messages
    ,@atomo-font-lock-keywords)
  "Simplified and adjusted highlighting matchers for the interaction.")

(defun atomo-inf-mode ()
  "Major mode for interacting Atomo subprocesses.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{atomo-inf-mode-map}

Entry to this mode calls the value of `atomo-inf-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `shell-mode-hook'.
`atomo-inf-mode-hook' is called after `shell-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp atomo-prompt-line-regexp)
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-read-only t)
  (setq major-mode 'atomo-inf-mode)
  (setq mode-name "Atomo Interaction")
  (use-local-map atomo-inf-mode-map)
  (make-local-variable 'mode-status)
  (set-syntax-table atomo-mode-syntax-table)
  (setq font-lock-defaults '(atomo-inf-font-lock-keywords))
  (font-lock-mode 1)
  ;(lazy-lock-mode 1)
  (setq atomo-output-buffer nil)
  (setq mode-status "Starting Up")
  (run-hooks 'comint-mode-hook 'atomo-inf-mode-hook))

(defun atomo (&optional cmd)
  "Starting an inferior Atomo process.
   Input and output via buffer `inferior-atomo-buffer-name'."
  (interactive
   (list (or atomo-cmd
         (read-from-minibuffer "Atomo toplevel to run: " atomo-cmd))))
  (if (eq major-mode 'atomo-inf-mode)
      (apply 'inf-atomo atomo-cmd atomo-args)
    (switch-to-buffer-other-window
     (apply 'inf-atomo atomo-cmd atomo-args)))
  (atomo-inf-mode)
  (setq list-buffers-directory atomo-dir)
  (setq *atomo-process* (get-buffer-process (current-buffer))))

(defun atomo-workspace ()
  "Create a scratch workspace buffer `*atomo-scratch*' for Atomo expressions."
  (interactive)
  (let ((buffer (get-buffer-create "*atomo-scratch*")))
    (save-excursion
      (set-buffer buffer)
      (atomo-mode)
      (setq mode-name "Atomo Workspace")
      (font-lock-mode))
    (pop-to-buffer "*atomo-scratch*")))

(defun atomo-run-overlay-at-point ()
  (interactive)
  (let* ((overlays (overlays-at (point)))
     (first-overlay (car overlays))
     (code (buffer-substring-no-properties (overlay-start first-overlay)
                    (overlay-end first-overlay))))
    (goto-char (point-max))
    (insert code)
    (atomo-send-input code)))

(defun atomo-follow-name-at-point ()
  "Follows a file reference of the form filename:linenumber at/after the point."
  (interactive)
  (push-mark)
  (let ((end))
    (let ((filename
           (save-excursion
             (skip-chars-forward "^:")
             (setq end (point))
             (skip-chars-backward "^ ")
             (buffer-substring-no-properties (point) end)))
          (line-number
           (save-excursion
             (skip-chars-forward "^:")
             (forward-char)
             (string-to-number (buffer-substring-no-properties (point) (progn (forward-word 1) (point)))))))
      (find-file filename)
      (goto-line line-number))))

(defun inf-atomo (cmd &rest args)
  "Run an inferior Atomo process `*atomo-process*'.
   Input and output via buffer `inferior-atomo-buffer-name'."
  (let ((buffer (get-buffer-create inferior-atomo-buffer-name))
    proc status)
    (when (setq proc (get-buffer-process buffer))
      (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      (cd atomo-dir)
      (unless (memq status '(run stop))
    (when proc (delete-process proc))
    (setq proc
          (if (equal window-system "x")
          (apply 'start-process
             cmd buffer
             "env"
             (format "TERMCAP=emacs:co#%d:tc=unknown:"
                 (frame-width))
             "TERM=emacs" "EMACS=t"
             cmd args)
        (apply 'start-process cmd buffer cmd args)))
    (setq cmd (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (set-process-filter proc 'atomo-inf-filter)
      ;(set-process-sentinel proc 'atomo-inf-sentinel)
      (atomo-inf-mode))
    buffer))

(defun atomo-handle-command (str)
  (eval (read str)))

(defun atomo-accum-command (string)
  (let (where)
    (setq where (string-match "\C-a" string))
    (setq atomo-output-buffer
      (concat atomo-output-buffer (substring string 0 where)))
    (if where
    (progn
      (unwind-protect        ;found the delimiter...do it
          (atomo-handle-command atomo-output-buffer)
        (setq atomo-output-buffer nil))
      ;; return the remainder
      (substring string where))
      ;; we ate it all and didn't do anything with it
      nil)))

;(defun atomo-inf-sentinel ())
(when (featurep 'xemacs)
  (require 'overlay))

(defun atomo-overlay (begin end face mouse-face action help)
  (let ((the-overlay (make-overlay begin end)))
    (overlay-put the-overlay 'face face)
    (overlay-put the-overlay 'mouse-face mouse-face)
    (overlay-put the-overlay 'help-echo help)
    the-overlay))

(defun atomo-inf-filter (process string)
  "Make sure that the window continues to show the most recently output
text."
  (with-current-buffer (process-buffer process)
    (when (buffer-name (current-buffer))
      (let ((where 0) ch command-str
        (moving (= (point) (process-mark process))))
    (while (and string where)
      (when atomo-output-buffer
        (setq string (atomo-accum-command string)))
      (when (and string
             (setq where (string-match "\C-a\\|\C-b" string)))
        (setq ch (aref string where))
        (cond ((= ch ?\C-a)        ;strip these out
           (setq string (concat (substring string 0 where)
                    (substring string (1+ where)))))
          ((= ch ?\C-b)               ;start of command
           (setq atomo-output-buffer "") ;start this off
           (setq string (substring string (1+ where)))))))
    (save-excursion
      (goto-char (point-max))
      (when string
        (setq mode-status "Idle")
        (insert string))
      (save-excursion     ; Handle most recent debugger output:
        (goto-char (point-max))
        (re-search-backward "^Debugging: " nil t)
        (save-excursion        ; Handle debugger file references:
          (let (fileref-end)
        (while (setq fileref-end (re-search-forward atomo-debug-fileref-regexp nil t))
          (let ((fileref-overlay (atomo-overlay (match-beginning 1) fileref-end 'link 'highlight nil "mouse-1: visit this file and line")))
            (overlay-put fileref-overlay 'keymap atomo-fileref-keymap)))))
                    ; Handle debugger frame references:
        (while (re-search-forward atomo-debug-frame-regexp nil t)
          (let ((frameref-overlay (atomo-overlay (match-beginning 1) (match-end 1) 'button nil nil "mouse-1: navigate to this frame")))
        (overlay-put frameref-overlay 'keymap atomo-frameref-keymap)))
                    ; Handle debugger restart listing:
        (let (restart-end)
          (while (setq restart-end (re-search-forward atomo-debug-restart-regexp nil t))
        (let ((restart-overlay (atomo-overlay (match-beginning 0) restart-end 'button nil nil "mouse-1: select this restart")))
          (overlay-put restart-overlay 'keymap atomo-restart-keymap)))))
      (when (process-mark process)
        (set-marker (process-mark process) (point-max))))
    (if moving (goto-char (process-mark process)))
    (sit-for 0)
    (set-window-point (get-buffer-window (current-buffer)) (point-max))))))

(defun atomo-inf-filter-redirect (process string)
  )

(defvar atomo-interactor-mode-map
  (let ((map (copy-keymap atomo-mode-map)))
    (mapcar
     #'(lambda (l) (define-key map (car l) (cdr l)))
     '(("\C-m"      . 'comint-send-input)
       ("\C-c\C-d"  . comint-delchar-or-maybe-eof)
       ("\C-c\C-u"  . comint-kill-input)
       ("\C-c\C-c"  . comint-interrupt-subjob)
       ("\C-c\C-z"  . comint-stop-subjob)
       ("\C-c\C-\\" . comint-quit-subjob)
       ("\C-c\C-o"  . comint-kill-output)
       ("\C-c\C-r"  . comint-show-output)))
    map)
  "Keymap for controlling the Atomo listener")

(defun atomo-ensure-running ()
  (unless (comint-check-proc inferior-atomo-buffer-name)
    (atomo)))

(defun atomo-eval-region (start end)
  "Send the current region to the inferior Atomo process. A stop character (a period) will be added to the end if necessary."
  (interactive "r")
  (atomo-ensure-running)
  (save-excursion
    (goto-char end)
    (atomo-backward-whitespace)
    (atomo-send-input (buffer-substring-no-properties start (point)))
    (display-buffer inferior-atomo-buffer-name t)))

(defun atomo-macroexpand-region (start end)
  "Send the current region to the inferior Atomo process, quoted, with a macroExpand call to get the macroExpand'd result."
  (interactive "r")
  (atomo-ensure-running)
  (save-excursion
    (goto-char end)
    (atomo-backward-whitespace)
    (comint-send-string
     inferior-atomo-buffer-name
     "`(")
    (comint-send-region inferior-atomo-buffer-name start (point))
    (comint-send-string
     inferior-atomo-buffer-name
     ") macroExpand.\n")
    (display-buffer inferior-atomo-buffer-name t)))

(defun atomo-print (start end)
  "Performs `atomo-eval-region' on the current region and inserts the output
into the current buffer after the cursor."
  (interactive "r")
  (atomo-ensure-running)
  (set-process-filter *atomo-process*
              (lambda (proc string) (insert string)))
  (save-excursion
    (goto-char end)
    (atomo-backward-whitespace)
    (atomo-send-input (buffer-substring-no-properties start (point)) t)
    (accept-process-output *atomo-process*))
  (set-process-filter *atomo-process* 'atomo-inf-filter))

(defun atomo-quit ()
  "Terminate the Atomo session and associated process."
  (interactive)
  (setq mode-status "Quitting")
  (comint-send-eof))

(defun atomo-snapshot (filename)
  "Save a Atomo snapshot."
  (interactive "FSnapshot name to save:")
  (setq mode-status "Saving")
  (atomo-send-input (format "Image saveNamed: '%s'"
                (expand-file-name filename))))

(defun atomo-filein (filename)
  "Do a load: on FILENAME."
  (interactive "FAtomo file to load: ")
  (atomo-ensure-running)
  (setq mode-status "Loading")
  (atomo-send-input (format "load: '%s'" (expand-file-name filename))))

(defun atomo-unit-tests (filename)
  "Load the unit-test file for the current file and run the tests."
  (interactive "FUnit-test file to load: ")
  (atomo-filein filename)
  (setq mode-status "Running tests")
  (atomo-send-input (format "load: '%s'" (expand-file-name filename)))
  (atomo-send-input "Tests CurrentUnit testSuite"))

(defun atomo-send-input (string &optional hide-p)
  (atomo-ensure-running)
  (set-buffer (get-buffer-create inferior-atomo-buffer-name))
  (unless hide-p
    (save-excursion
      (point-max)
      (insert string)
      (insert (if (and (>= (point) 2) (eq (preceding-char) ?.)) "\n" ".\n"))))
  (setq mode-status "Running")
  (comint-send-string inferior-atomo-buffer-name string)
  (comint-send-string
   inferior-atomo-buffer-name
   (if (and (>= (point) 2) (eq (preceding-char) ?.)) "\n" ".\n")))

;; (defun atomo-send (str &optional mode)
;;   (let (temp-file buf old-buf)
;;     (setq temp-file (concat temporary-file-directory (make-temp-name "atomo")))
;;     (save-excursion
;;       (setq buf (get-buffer-create " zap-buffer "))
;;       (set-buffer buf)
;;       (erase-buffer)
;;       (princ str buf)
;;       (write-region (point-min) (point-max) temp-file nil 'no-message))
;;     (kill-buffer buf)
;;     ;; this should probably be conditional
;;     (save-window-excursion (atomo atomo-args))
;;     (setq old-buf (current-buffer))
;;     (setq buf (process-buffer *atomo-process*))
;;     (pop-to-buffer buf)
;;     (when mode
;;       (setq mode-status mode))
;;     (goto-char (point-max))
;;     (newline)
;;     (pop-to-buffer old-buf)
;;     (comint-send-string *atomo-process*
;;                (format "load: '%s'.\n" temp-file))))

;; Indentation
;; ===========

;; Basic utilities: rely on only basic Emacs functions.

(defun atomo-comment-indent ()
  "This is used by `indent-for-comment' to decide how much to indent a comment
in Atomo code based on its context."
  (if (looking-at "^\"")
      0                ;Existing comment at bol stays there.
      (save-excursion
        (skip-chars-backward " \t")
        (max (1+ (current-column))    ;Else indent at comment column
             comment-column))))    ; except leave at least one space.

(defun atomo-indent-to-column (col)
  (save-excursion
    (beginning-of-line)
    (indent-line-to col))
  (when (< (current-column) col)
    (move-to-column col)))

(defun atomo-current-column ()
  "Returns the current column of the given line, regardless of narrowed buffer."
  (save-restriction
    (widen)
    (current-column)))

(defun atomo-previous-nonblank-line ()
  (forward-line -1)
  (while (and (not (bobp))
          (looking-at "^[ \t]*$"))
    (forward-line -1)))

(defun atomo-in-string ()
  "Returns non-nil delimiter as a string if the current location is
actually inside a string or string like context."
  (let (state)
    (setq state (parse-partial-sexp (point-min) (point)))
    (and (nth 3 state)
     (char-to-string (nth 3 state)))))

(defun atomo-white-to-bolp ()
  "Returns T if from the current position to beginning of line is whitespace."
  (let (done is-white line-start-pos)
    (save-excursion
      (save-excursion
    (beginning-of-line)
    (setq line-start-pos (point)))
      (while (not done)
    (unless (bolp)
      (skip-chars-backward " \t"))
    (cond ((bolp)
           (setq done t)
           (setq is-white t))
          ((eq (char-after (1- (point))) ?\")
           (backward-sexp)
           (when (< (point) line-start-pos) ;comment is multi line
         (setq done t)))
          (t
           (setq done t))))
      is-white)))

(defun atomo-backward-comment ()
  "Moves to the beginning of the containing comment, or
the end of the previous one if not in a comment."
  (search-backward "\"")        ;find its start
  (while (eq (preceding-char) ?\\)    ;skip over escaped ones
    (backward-char 1)
    (search-backward "\"")))

;; Basic utilities that use `atomo-mode' variables.

(defun atomo-forward-whitespace ()
  "Skip white space and comments forward, stopping at end of buffer
or non-white space, non-comment character."
  (while (looking-at (concat "[" atomo-whitespace-chars "\"]"))
    (skip-chars-forward atomo-whitespace-chars)
    (when (eq (following-char) ?\")
      (forward-sexp))))

(defun atomo-backward-whitespace ()
  "Like `atomo-forward-whitespace' only going towards the start of the buffer."
  (while (progn (skip-chars-backward atomo-whitespace-chars)
        (eq (preceding-char) ?\"))
    (backward-sexp)))

(defun atomo-tab ()
  "This gets called when the user hits [tab] in a `atomo-mode' buffer."
  (interactive)
  (let (col)
    ;; round up, with overflow
    (setq col (* (/ (+ (current-column) atomo-indent-increment)
            atomo-indent-increment)
         atomo-indent-increment))
    (indent-to-column col)))

;; Higher-level utilities calling `atomo-mode' functions.

(defun atomo-forward-sexp (&optional n)
  "Move forward N Atomo expressions."
  (interactive "p")
  (unless n (setq n 1))
  (cond ((< n 0)
         (atomo-backward-sexp (- n)))
        ((null parse-sexp-ignore-comments)
         (forward-sexp n))
        (t
         (while (> n 0)
           (atomo-forward-whitespace)
           (forward-sexp)
           (decf n)))))

(defun atomo-backward-sexp (&optional n)
  "Move backward N Atomo expressions."
  (interactive "p")
  (unless n (setq n 1))
  (cond ((< n 0)
         (atomo-forward-sexp (- n)))
        ((null parse-sexp-ignore-comments)
         (backward-sexp n))
        (t
         (while (> n 0)
           (atomo-backward-whitespace)
           (backward-sexp)
           (decf n)))))

(defun atomo-find-statement-begin ()
  "Leaves the point at the first non-blank, non-comment character of a new
statement.  If beginning of buffer is reached, then the point is left there.
This routine only will return with the point pointing at the first non-blank
on a line; it won't be fooled by multiple statements on a line into stopping
prematurely.  Also, goes to start of method if we started in the method
selector."
  (let (start ch)
    (when (eq (preceding-char) ?.) ; if we start at eos
      (backward-char 1))          ; we find the begin of THAT stmt
    (while (and (null start) (not (bobp)))
      (atomo-backward-whitespace)
      (setq ch (preceding-char))
      (cond ((eq ch ?.)
         (let (saved-point)
           (setq saved-point (point))
           (atomo-forward-whitespace)
           (if (atomo-white-to-bolp)
           (setq start (point))
         (goto-char saved-point)
         (atomo-backward-sexp 1))))
        ((eq ch ?^)     ; HACK -- presuming that when we back
                    ;up into a return that we're at the
                    ;start of a statement
         (backward-char 1)
         (setq start (point)))
        ((eq ch ?\[)
         (if (> (current-column) 1)
         (setq start (point))
           (forward-line 1)
           (beginning-of-line)
           (atomo-forward-whitespace)
           (setq start (point))))
        ((eq ch ?\{)
         (setq start (point)))
        ((eq ch ?\()
         (backward-char 1))
        ((eq ch ?|)
         (backward-char 1)
         (skip-chars-backward "^[")
         ;(atomo-backward-whitespace)
         )
        (t
         (atomo-backward-sexp 1))))
    (unless start
      (goto-char (point-min))
      (atomo-forward-whitespace)
      (setq start (point)))
    start))

(defun atomo-calculate-indent ()
  "The core calculations for indentation."
  (let (indent-amount start-of-line state (parse-sexp-ignore-comments t))
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (point-min) (point)) ;only care about what's before
        (setq state (parse-partial-sexp (point-min) (point)))
        (cond ((eq (nth 3 state) ?\") ;in a comment
               (save-excursion
                 (atomo-backward-comment)
                 (setq indent-amount (+ (current-column) atomo-indent-increment))))
              ((eq (nth 3 state) ?') ;in a string
               (setq indent-amount 0))
              ((eq (nth 3 state) ?\))
               (setq indent-amount (+ (current-column) atomo-indent-increment))))
        (unless indent-amount
          (atomo-narrow-to-method)
          (beginning-of-line)
          (setq state (parse-partial-sexp (point-min) (point)))
          (atomo-narrow-to-paren state)
          (atomo-backward-whitespace)
          (cond ((bobp)         ;must be first statement in block or exp
                 (if (nth 1 state)        ;within a paren exp
                     (save-restriction
                       (widen)
                       (beginning-of-line)
                       (atomo-forward-whitespace)
                       (setq indent-amount (+ (atomo-current-column)
                                              atomo-indent-increment)))
                   ;; we're top level
                   (setq indent-amount atomo-indent-increment)))
                ((equal (nth 0 state) 0)  ;at top-level
                 (beginning-of-line)
                 (atomo-forward-whitespace)
                 (setq indent-amount (atomo-current-column)))
                ((eq (preceding-char) ?.) ;at end of statement
                 (atomo-find-statement-begin)
                 (setq indent-amount (atomo-current-column)))
                ((eq (preceding-char) ?|)
                 (atomo-find-statement-begin)
                 (setq indent-amount (atomo-current-column)))
                ((eq (preceding-char) ?:)
                 (beginning-of-line)
                 (atomo-forward-whitespace)
                 (setq indent-amount (+ (atomo-current-column)
                                        atomo-indent-increment)))
                (t                     ;must be a statement continuation
                 (atomo-find-statement-begin)
                 (setq indent-amount (+ (atomo-current-column)
                                        atomo-indent-increment)))))))
    (save-excursion
      (save-restriction
        (widen)
        (beginning-of-line)
        (skip-chars-forward " \t")
        (when (memq (following-char) '(?\} ?\) ?\]))
          (setq indent-amount (max 0 (- indent-amount atomo-indent-increment))))
        (while (memq (following-char) '(?\} ?\) ?\]))
          (setq indent-amount (max 0 (- indent-amount atomo-indent-increment)))
          (forward-char))))
    (when (= indent-amount 1) (setq indent-amount atomo-indent-increment))
    indent-amount))

(defun atomo-indent-line ()
  "Sees if the current line is a new statement, in which case indent the same
as the previous statement, or determine by context. If not the start of a new
statement, the start of the previous line is used, except if that starts a
new line, in which case it indents by `atomo-indent-increment'."
  (let (indent-amount is-keyword)
    (save-excursion
      (beginning-of-line)
      (atomo-forward-whitespace)
      (when (looking-at "[A-Za-z][A-Za-z0-9]*:") ;indent for colon
        (let ((parse-sexp-ignore-comments t))
          (beginning-of-line)
          (atomo-forward-whitespace)
          (unless (memq (preceding-char) '(?. ?| ?\[ ?\( ?\{))
            (setq is-keyword t)))))
      (setq indent-amount (atomo-calculate-indent))
      (atomo-indent-to-column indent-amount)))

(defun atomo-reindent ()
  (interactive)
  ;; Still loses if at first character on line
  (atomo-indent-line))

(defun atomo-newline-and-indent ()
  (interactive "p")
  (newline)
  (atomo-indent-line))

(defun atomo-begin-of-defun ()
  "Skip to the beginning of the current method.
If already at the beginning of a method, skips to the beginning of the
previous one."
  (interactive)
  (let ((parse-sexp-ignore-comments t) here delim start)
    (setq here (point))
    (while (and (search-backward "@" nil 'to-end)
        (setq delim (atomo-in-string)))
      (search-backward delim))
    (setq start (point))
    (beginning-of-line)
    (atomo-forward-whitespace)
    ;; check to see if we were already at the start of a method
    ;; in which case, the semantics are to go to the one preceding
    ;; this one
    (when (and (= here (point))
           (/= start (point-min)))
      (goto-char start)
      (atomo-backward-whitespace)    ;may be at ! "foo" !
      (when (eq (preceding-char) ?@)
;    (backward-char)
    (beginning-of-line)
    (atomo-forward-whitespace)
    (atomo-backward-sexp 1))
      (atomo-begin-of-defun))))        ;and go to the next one

(defun atomo-narrow-to-paren (state)
  "Narrows the region to between point and the closest previous opening bracket.
It also skips over block headers, and following whitespace on the same line."
  (let ((paren-addr (nth 1 state))
        start c)
    (when paren-addr
      (save-excursion
        (goto-char paren-addr)
        (setq c (following-char))
        (when (memq c '(?\( ?\{ ?\[))
          (setq start (if (> (point) 2) (1+ (point)) 0))))
      (narrow-to-region start (point)))))

(defun atomo-at-method-begin ()
  "Return T if at the beginning of a block, otherwise nil"
  (let ((parse-sexp-ignore-comments t))
    (when (bolp)
      (save-excursion
        (atomo-backward-whitespace)
        (eq (preceding-char) ?\[)))))

(defun atomo-colon ()
  "Possibly reindents a line when a colon is typed.
If the colon appears on a keyword that's at the start of the line (ignoring
whitespace, of course), then the previous line is examined to see if there
is a colon on that line, in which case this colon should be aligned with the
left most character of that keyword.  This function is not fooled by nested
expressions."
  (interactive)
  (self-insert-command 1)
  (let (needs-indent state (parse-sexp-ignore-comments t))
    (setq state (parse-partial-sexp (point-min) (point)))
    (unless (nth 3 state)        ;unless in string or comment
      (save-excursion
    (skip-chars-backward atomo-name-chars)
    (when (and (looking-at atomo-name-regexp)
           (not (atomo-at-method-begin)))
      (setq needs-indent (atomo-white-to-bolp))))
      (when needs-indent
    (atomo-indent-for-colon)))))

(defun atomo-indent-for-colon ()
  "Called only for lines which look like `<whitespace>foo:'."
  (let (indent-amount c state done default-amount start-line
              (parse-sexp-ignore-comments t))
    (save-excursion
      (save-restriction
    (widen)
    (atomo-narrow-to-method)
    (beginning-of-line)
    (setq state (parse-partial-sexp (point-min) (point)))
    (narrow-to-region (point-min) (point))
    (setq start-line (point))
    (atomo-backward-whitespace)
    (cond
     ((bobp)
      (setq indent-amount (atomo-current-column)))
     ((or (eq (setq c (preceding-char)) ?|)
          (eq c ?\[))        ; method header before
      (skip-chars-backward "^[")
      (atomo-find-statement-begin)
      (setq indent-amount (atomo-current-column)))
     ((eq c ?.)    ; stmt end, indent like it
      (atomo-find-statement-begin)
      (setq indent-amount (atomo-current-column)))
     (t                ;could be a winner
      (atomo-find-statement-begin)
      ;; we know that since we weren't at bobp above after backing
      ;; up over white space, and we didn't run into a ., we aren't
      ;; at the beginning of a statement, so the default indentation
      ;; is one level from statement begin
      (setq default-amount
        (+ (atomo-current-column) ;just in case
           atomo-indent-increment))
      ;; might be at the beginning of a method (the selector), decide
      ;; this here
      (unless (looking-at atomo-keyword-regexp)
        ;; not a method selector
        (while (and (not done) (not (eobp)))
          (atomo-forward-sexp 1) ;skip over receiver
          (atomo-forward-whitespace)
          (unless (and indent-amount ;pick up only first one
               (not (looking-at atomo-keyword-regexp)))
        (setq indent-amount (atomo-current-column)))))
      (unless indent-amount
        (setq indent-amount default-amount))))))
    (when indent-amount
      (atomo-indent-to-column indent-amount))))

(defun atomo-narrow-to-method ()
  "Narrows the buffer to the contents and signature of the method."
  ; TODO: Make sure the signature plus optional head comment is included.
  (interactive)
  (let ((end (point))
    (parse-sexp-ignore-comments t)
    handled)
    (save-excursion
      (atomo-begin-of-defun)
      (if (looking-at "[A-Za-z]")    ;either unary or keyword msg
      ;; or maybe an immediate expression...
      (progn
        (forward-sexp)
        (if (eq (following-char) ?:) ;keyword selector
        (progn            ;parse full keyword selector
          (backward-sexp 1)    ;setup for common code
          (atomo-forward-keyword-selector))
          ;; else maybe just a unary selector or maybe not
          ;; see if there's stuff following this guy on the same line
          (let (here eol-point)
        (setq here (point))
        (end-of-line)
        (setq eol-point (point))
        (goto-char here)
        (atomo-forward-whitespace)
        (if (< (point) eol-point) ;if there is, we're not a method
            (beginning-of-line)
          (goto-char here)    ;else we're a unary method (guess)
          ))))
          ;; this must be a binary selector, or a temporary
          (when (eq (following-char) ?|)
        (end-of-line)    ;could be temporary
        (atomo-backward-whitespace)
        (when (eq (preceding-char) ?|)
          (setq handled t))
        (beginning-of-line))
          (unless handled
        (skip-chars-forward (concat "^" atomo-whitespace-chars))
        (atomo-forward-whitespace)
        (skip-chars-forward atomo-name-chars))) ;skip over operand
      (atomo-forward-whitespace)
      (if (eq (following-char) ?|)    ;scan for temporaries
      (progn
        (forward-char)        ;skip over |
        (atomo-forward-whitespace)
        (while (and (not (eobp))
            (looking-at "[A-Za-z]"))
          (skip-chars-forward atomo-name-chars)
          (atomo-forward-whitespace))
        (when (and (eq (following-char) ?|) ;if a matching | as a temp
               (< (point) end))    ;and we're after the temps
          (narrow-to-region (1+ (point)) end) ;we limit the buffer
          ))
    (when (< (point) end)
      (narrow-to-region (point) end))))))

(defun atomo-forward-keyword-selector ()
  "Starting on a keyword, this function skips forward over a keyword selector.
It is typically used to skip over the actual selector for a method."
  (interactive)
  (let (done)
    (while (not done)
      (if (not (looking-at "[A-Za-z_]"))
      (setq done t)
    (skip-chars-forward atomo-name-chars)
    (if (eq (following-char) ?:)
        (progn
          (forward-char)
          (atomo-forward-sexp)
          (atomo-forward-whitespace))
      (setq done t)
      (backward-sexp))))))

(defun atomo-collect-selector ()
  "Point is stationed inside or at the beginning of the selector in question.
This function computes the Atomo selector (unary, binary, or keyword) and
returns it as a string.  Point is not changed."
  (save-excursion
    (let (start selector done ch
        (parse-sexp-ignore-comments t))
      (skip-chars-backward (concat "^" "\"" atomo-whitespace-chars))
      (setq start (point)) ;back to whitespace
      (if (looking-at atomo-name-regexp)
      (progn            ;maybe unary, maybe keyword
        (skip-chars-forward atomo-name-chars)
        (if (eq (following-char) ?:)    ;keyword?
        (progn
          (forward-char 1)
          (setq selector (buffer-substring start (point)))
          (setq start (point))
          (while (not done)
            (atomo-forward-whitespace)
            (setq ch (following-char))
            (cond ((memq ch '(?@ ?\] ?\) ?.)) ;stop at end of expr
               (setq done t))
              ((eq ch ?:) ;add the next keyword
               (forward-char 1)
               (setq selector
                 (concat selector
                     (buffer-substring start (point)))))
              (t
               (setq start (point))
               (atomo-forward-sexp 1)))))
          (setq selector (buffer-substring start (point)))))
    (skip-chars-forward (concat "^\"" atomo-whitespace-chars))
    (setq selector (buffer-substring start (point))))
      selector)))

(defun atomo-collect-signature ()
  "Similar to atomo-collect-selector except that it looks for dispatching
annotations. It returns the selector string and the names of the arguments in
a list. Note that the first argument must be found by searching backwards."
  (save-excursion
    (let (start selector done ch arg-names
        (parse-sexp-ignore-comments t))
      (skip-chars-backward (concat "^" "\"" atomo-whitespace-chars))
      (setq start (point))
      (if (looking-at atomo-name-regexp)
      (progn            ;maybe unary, maybe keyword
        (skip-chars-forward atomo-name-chars)
        (if (eq (following-char) ?:)    ;keyword?
        (progn
          (forward-char 1)
          (setq selector (buffer-substring start (point)))
          (setq start (point))
          (while (not done)
            (atomo-forward-whitespace)
            (setq ch (following-char))
            (cond ((memq ch '(?@ ?\] ?\) ?.))
               (setq done t))
              ((eq ch ?:)
               (forward-char 1)
               (setq selector
                 (concat selector
                     (buffer-substring start (point)))))
              (t
               (setq start (point))
               (atomo-forward-sexp 1)))))
          (setq selector (buffer-substring start (point)))))
    (skip-chars-forward (concat "^" ?\" atomo-whitespace-chars))
    (setq selector (buffer-substring start (point))))
      selector)))

;; Imenu Support
;; =============

(defconst atomo-imenu-generic-expression
  `(("Slots" ,(format "^.*add[A-Za-z]*Slot: #\\(%s\\) valued: .* derive"
                      atomo-name-regexp) 1)
    ("Namespaces" ,(format "^\\(.*\\) ensureNamespace: #\\(%s\\).*\."
                           atomo-name-regexp) 2)
    ("Definitions" ,(format "^\\(.*\s*\\)?define\\(Prototype\\)?: #\\(%s\\)"
                            atomo-name-regexp) 2)
    ("Definitions" ,(format "#\\(%s\\) :?:="
                            atomo-name-regexp) 2)
    ("Methods" "^\\([^\[]*@[^\[\"]*\\)$" 1) ; Matches the whole signature.
    ))

(defun atomo-mode ()
  "Major mode for editing Atomo code.
Type `M-x atomo' to open a Atomo interaction area.
Notes:
`atomo-mode-hook' is activated on entering the mode.
\\{atomo-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'atomo-mode
        mode-name "Atomo")
  (use-local-map atomo-mode-map)
  (set-syntax-table atomo-mode-syntax-table)
  (setq font-lock-defaults '(atomo-font-lock-keywords))
  (mapc
   (lambda (l)
     (set (make-local-variable (car l)) (cdr l)))
   '((paragraph-start              . "^\\|$")
     (paragraph-separate           . "[ ^\\|]*$")
     (paragraph-ignore-fill-prefix . t)
     (indent-line-function         . atomo-indent-line)
     (require-final-newline        . t)
     (comment-start                . "\"")
     (comment-end                  . "\"")
     (comment-column               . 32)
     (comment-start-skip           . "\" *")
     (comment-indent-function      . atomo-comment-indent)
     (parse-sexp-ignore-comments   . nil)
     (local-abbrev-table           . atomo-mode-abbrev-table)
     ))
  (setq imenu-generic-expression atomo-imenu-generic-expression)
  (setq font-lock-verbose t)
  (run-hooks 'atomo-mode-hook))

(provide 'atomo-mode)

(add-to-list 'auto-mode-alist '("\\.atomo" . atomo-mode))
