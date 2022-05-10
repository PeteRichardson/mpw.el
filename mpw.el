;
; mpw.el --  MPW minor mode for emacs
;            Author: Pete Richardson
;            Copyright (C) 1997, WebTV Networks, Inc.
;

;-----------------------------------------------------------------
;    User Variables
;-----------------------------------------------------------------
(defvar mpw-mode nil
  "Mode variable for mpw minor mode.")


;-----------------------------------------------------------------
;    Public Functions
;-----------------------------------------------------------------
(defun mpw-mode (&optional arg)
  "mpw minor mode."
  (interactive "P")
  (setq mpw-mode
	(if (null arg)
	    (not mpw-mode)
	  (> (prefix-numeric-value arg) 0)))

; Set transient-mark-mode so the current selection is highlighted
; but first save the old value (and restore when leaving mpw-mode.
  (if mpw-mode
      (progn
	(defvar mpw-saved-transient-mark-mode transient-mark-mode)
	(transient-mark-mode t))
    (transient-mark-mode mpw-saved-transient-mark-mode))
)

; Set up the mode indicator for the mode banner.
(if (not (assq 'mpw-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(mpw-mode " MPW")
		minor-mode-alist)))


;-----------------------------------------------------------------
;    Keymap
;-----------------------------------------------------------------
(defvar mpw-mode-map nil
  "Keymap for MPW mode.")

; NOTE: functions that start with "mpw-" are defined in this file
(if mpw-mode-map
    nil    ; don't do anything if the map is already setup
  (setq mpw-mode-map (make-sparse-keymap))
  ; File menu
  (define-key mpw-mode-map "\M-n" 'mpw-new-buffer)     ; File:New...
  (define-key mpw-mode-map "\M-o" 'find-file)          ; File:Open...
  (define-key mpw-mode-map "\M-d" 'mpw-open-selection) ; File:Open Selection
  (define-key mpw-mode-map "\M-w" 'delete-frame)      ; File:Close
  (define-key mpw-mode-map "\M-s" 'save-buffer)        ; File:Save
  (define-key mpw-mode-map "\M-p" 'print-buffer)       ; File:Print Window
  (define-key mpw-mode-map "\M-q" 'save-buffers-kill-emacs) ; File:Quit

  ; Edit menu
  (define-key mpw-mode-map "\M-z" 'undo)	       ; Edit:Undo
  (define-key mpw-mode-map "\M-x" 'kill-region)        ; Edit:Cut
  (define-key mpw-mode-map "\M-c" 'kill-ring-save)     ; Edit:Copy
  (define-key mpw-mode-map "\M-v" 'mpw-paste)	       ; Edit:Paste
  (define-key mpw-mode-map "\M-b" 'delete-region)      ; Edit:Clear
  (define-key mpw-mode-map "\M-a" 'mark-whole-buffer)  ; Edit:Select all
  (define-key mpw-mode-map "\M-\[" 'mpw-shift-text-left)   ; Edit:Shift Left
  (define-key mpw-mode-map "\M-\]" 'mpw-shift-text-right)  ; Edit:Shift Right

  ; Find menu
  (define-key mpw-mode-map "\M-f" 'mpw-nonincremental-search-forward) ; Find:Find...
  (define-key mpw-mode-map "\M-g" 'mpw-nonincremental-repeat-search-forward) ; Find:Find Same
  (define-key mpw-mode-map "\M-h" 'mpw-find-selection) ; Find:Find Selection
  (define-key mpw-mode-map "\M-r" 'query-replace)      ; Find:Replace...

  ; p4 menu
  (define-key mpw-mode-map "\M-6" 'mpw-update)         ; p4:Update to latest
  (define-key mpw-mode-map "\M-7" 'mpw-changes)        ; p4:Prepare Notes for Commit
  (define-key mpw-mode-map "\M-8" 'mpw-commit)         ; p4:Commit Files in Notes
  (define-key mpw-mode-map "\M-*" 'p4-edit)            ; p4:Edit Active
  (define-key mpw-mode-map "\M-+" 'p4-revert)          ; p4:Revert Active
  (define-key mpw-mode-map "\M--" 'mpw-blah)           ; p4:Commit Active

  ; Misc bindings
  ;  Since we replaced M-x, we need a new binding for execute-extended-command
  (define-key mpw-mode-map "\M-\r" 'execute-extended-command)
  ;  Deleting the selection with the delete/backspace key is very handy
  (define-key mpw-mode-map "\C-?" 'mpw-super-delete)

  (setq minor-mode-map-alist
	(cons (cons 'mpw-mode mpw-mode-map) minor-mode-map-alist))
)


;-----------------------------------------------------------------
;    Private Functions
;-----------------------------------------------------------------
(defun mpw-blah ()
  "test the key binding"
  (interactive)
  (message "blah!")
)


; actually, we can do this with new frames...
; see find-file-other-frame for open and delete-frame for close
(defun mpw-new-buffer ()
  "Create a new buffer called Untitled<n>"
  (interactive)
 (setq new-buffer-name (generate-new-buffer-name "Untitled"))
;  (generate-new-buffer new-buffer-name)
;  (switch-to-buffer new-buffer-name))
  (find-file-other-frame new-buffer-name)
  (switch-to-buffer-other-frame new-buffer-name))

(defun mpw-open-selection ()
  "Opens a file whose name is currently selected"
  (interactive)
  (if mark-active
      (save-excursion
	(if (> (mark) (point))
	    (exchange-point-and-mark))
	(find-file (buffer-substring (mark) (point)))
	)
    (message "Select a filename first")
 )
)

(defun mpw-paste ()
  "pastes the contents of the kill-ring into the buffer,
   deleting the selection if there is one"
  (interactive)
  (if mark-active
      (delete-region (mark) (point)))
  (yank)
)

(defun mpw-nonincremental-search-forward (string)
  "Read a string and search for it nonincrementally."
  (interactive "sSearch for string: ")
  (if (not (equal string ""))
      (isearch-update-ring string nil)
    (setq string (car search-ring)))
    (let ((string-length (length string))
	  (found-string  (search-forward string nil nil)))
      (if found-string
	  (push-mark (- found-string string-length) t t)
	)
      )
    )

(defun mpw-nonincremental-repeat-search-forward ()
  "Search forward for the previous search string."
  (interactive)
  (let ((string-length (length (car search-ring)))
	(found-string  (search-forward (car search-ring) nil nil)))
    (if found-string
	(push-mark (- found-string string-length) t t)
      )
    )
  )

(defun mpw-find-selection ()
  "Finds the current selection in the buffer"
  (interactive)
  (if mark-active
      (let ((string-length (abs (- (mark) (point)))))
	(save-excursion
	  (if (> (mark) (point))
	      (exchange-point-and-mark))
	  (message "searching for \"%s\"..." (buffer-substring (mark) (point)))
	  )
	(let ((found-string
	       (search-forward (buffer-substring (mark) (point)) nil nil)))
	  (if found-string
	    (push-mark (- found-string string-length) t t)
	  )
	(message "Select some text to find first")
	)
	)
    )
)

(defun mpw-super-delete ()
  "delete previous char or selection"
  (interactive)
  (if mark-active
      (delete-region (mark) (point))
    (backward-delete-char-untabify 1)))

(defun mpw-shift-text-left ()
  "shift text left"
  (interactive)
  (shift-text -1)
)

(defun mpw-shift-text-right ()
  "shift text right"
  (interactive)
  (shift-text 1)
)

(defun mpw-shift-text (direction)
  (if mark-active
      (save-excursion
	(if (< (mark) (point))
	    (exchange-point-and-mark))
	(beginning-of-line)
	(exchange-point-and-mark)
	(indent-rigidly (mark) (point) (* direction tab-width))
	)
    (message "Select some lines to shift first")
    )
  )

(defun mpw-update ()
  "Update to Latest"
  (interactive)
  (start-process-shell-command "update-process" "*scratch*" "update")
)

(defun mpw-changes ()
  "Update to Latest"
  (interactive)
  (start-process-shell-command "update-process" "*scratch*" "changes")
)

(defun mpw-commit ()
  "Update to Latest"
  (interactive)
  (start-process-shell-command "update-process" "*scratch*" "commit")
)
