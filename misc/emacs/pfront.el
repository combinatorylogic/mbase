;;; pfront.el --- Pfront major mode

;; Author: Wojciech Meyer <wojciech.meyer@gmail.com>
;; Keywords: extensions

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defvar pfront-suppress nil)

(defgroup pfront nil
  "Run Pfront."
  :group 'comm)

(defcustom pfront-buffer-name "*Pfront*"
  "Name of the buffer used to run pfront."
  :group 'pfront
  :type 'string)

(defcustom pfront-command "pfhighlight"
  "Command to run to launch Pfront."
  :group 'pfront
  :type 'string)

(defvar pfront-comint-buffer nil)
;; (defun pfront-make-buffer ()
;;   "Get the pfront buffer."
;;   (let ((buffer (get-buffer-create pfront-buffer-name)))
;;     (with-current-buffer buffer
;;       (comint-mode))
;;     buffer))

(define-derived-mode pfront-inferior-mode comint-mode "Pfront"
  "A major mode for Pfront interaction."
  :group 'comm
  (setq buffer-read-only t)
  (setq buffer-undo-list t))

(defvar pfront-lastfailure nil)

(defvar pfront-processed-buffer nil)

(defun exec_indent (msg sc lst)
  (if (eq (car msg) 'push)
      (let* ((offset (if (eq (cadr msg) 'firsttab) 0 2)))
        (cons (+ sc offset) lst))
    (if lst (cdr lst) nil)))

(defun get_indent_list (start-line)
  (save-excursion
    (goto-char (point-min))
    (forward-line start-line)
    (forward-char 0)
    (let* ((pos (point))
           (ilist (get-text-property pos 'pfront_indent)))
      ilist)))

(defun get_prev_indent_list (ln)
  (if (> ln 0)
      (let* ((chk (get_indent_list ln)))
        (if chk chk
          (get_prev_indent_list (- ln 1))))
    nil))

(defun pfront-process-single (mess)
  (ignore-errors
    (let* ((msg (read mess))
           (lns (car msg)))
      (if (symbolp lns)
          (cond
           ((eq lns 'FAILURE-POSITION)
            (let* ((m0 (car (cdr msg)))
                   (start-line (car m0))
                   (start-col (car (cdr m0)))
                   (hint (car (cdr (cdr msg))))
                   )
              (message (concat "Error at: " hint))
              (save-excursion
                (goto-char (point-min)) (forward-line start-line)
                (let ((start-pos (point)))
                  (end-of-line)
                  (let ((end-pos (point)))

                    (put-text-property start-pos end-pos 'display 'font-lock-comment-face)
                    (put-text-property start-pos end-pos 'face 'underline)
                    (setq pfront-lastfailure (list start-pos end-pos))
                    )))))
           (else
            (message (concat "SYM: " mess))))
        (let* (
               (alns (car lns))
               (adlns (car (cdr lns)))
               (start-line (car alns))
               (start-col (car (cdr alns)))
               (end-line (car adlns))
               (end-col (car (cdr adlns)))
               (token-type (car (cdr (car (cdr msg)))))
               )
          (if (eq (car (car (cdr msg))) 'indent)
              (let* ((ilist (get_prev_indent_list start-line))
                     (np (cons (exec_indent token-type
                                            start-col ilist))))
                (put-text-property pos 'pfront_indent np)

                )
              ; else
              (let (
                    (color-face
                     (cond ((eq token-type 'comment) 'font-lock-comment-face)
                           ((eq token-type 'keyword) 'font-lock-function-name-face)
                           ((eq token-type 'const) 'font-lock-keyword-face)
                           ((eq token-type 'lexic) 'font-lock-keyword-face)
                           ((eq token-type 'ident) nil) ;; 'font-lock-function-name-face)
                           ((eq token-type 'forall) '("∀"))
                           ((eq token-type 'exist) '("∃"))

                           ((eq token-type 'to) '("↦"))
                           ((eq token-type 'doubleto) '("⟹"))
                           ((eq token-type 'leftset) '("⟸"))
                           ((eq token-type 'leftright) '("↔"))
                           ((eq token-type 'lambda) '("λ"))
                           ((eq token-type 'emptyset) '("∅"))
                           ((eq token-type 'append) '("⊕"))
                           ((eq token-type 'infty) '("∞"))

                           ((eq token-type 'Delta) '("∆"))
                           ((eq token-type 'logand) '("∧"))
                           ((eq token-type 'neq) '("≠"))
                           ((eq token-type 'logor) '("∨"))
                           ((eq token-type 'eltof) '("∈"))
                           ((eq token-type 'noteltof) '("∉"))
                           ((eq token-type 'intersect) '("∩"))
                           ((eq token-type 'union) '("∪"))
                           ((eq token-type 'ring) '("∘"))


                           ((eq token-type 'alpha) '("α"))
                           ((eq token-type 'beta) '("β"))
                           ((eq token-type 'gamma) '("γ"))
                           ((eq token-type 'delta) '("δ"))
                           ((eq token-type 'epsilon) '("ε"))
                           ((eq token-type 'zeta) '("ζ"))
                           ((eq token-type 'eta) '("η"))
                           ((eq token-type 'theta) '("θ"))
                           ((eq token-type 'iota) '("ι"))
                           ((eq token-type 'kappa) '("κ"))
                           ((eq token-type 'greek-lambda) '("λ"))
                           ((eq token-type 'mu) '("μ"))
                           ((eq token-type 'nu) '("ν"))
                           ((eq token-type 'xi) '("ξ"))
                           ((eq token-type 'omicron) '("ο"))
                           ((eq token-type 'pi) '("π"))
                           ((eq token-type 'rho) '("ρ"))
                           ((eq token-type 'sigma) '("σ"))
                           ((eq token-type 'tau) '("τ"))
                           ((eq token-type 'upsilon) '("υ"))
                           ((eq token-type 'phi) '("φ"))
                           ((eq token-type 'chi) '("χ"))
                           ((eq token-type 'psi) '("ψ"))
                           ((eq token-type 'omega) '("ω"))

                           ((eq token-type 'sum) '("∑"))

                           (else nil))))
                (save-excursion
                  (goto-char (point-min)) (forward-line start-line)
                  (forward-char start-col)
                  (let ((start-pos (point)))
                    (goto-char (point-min)) (forward-line end-line)
                    (forward-char end-col)
                    (let ((end-pos (point)))
                      (cond
                       ((null color-face) nil)
                       ((listp color-face)
                        (progn
                          (put-text-property start-pos end-pos 'display (car color-face))
                          (put-text-property start-pos end-pos 'face 'mathsymbol)
                          ))
                       ((symbolp color-face)
                        (put-text-property start-pos end-pos 'face color-face))
                       )))))))))))

(defvar pfront-process-buffer "")

(defun pfront-process-filter (process mess)
  "Filter PROCESS output MSG."
  (setq pfront-process-buffer (concat pfront-process-buffer mess))
  (let* ((lstc (substring pfront-process-buffer -2 -1)))
    (if (string= lstc "|")
        (let* ((l1 (split-string pfront-process-buffer "|")))
          (setq pfront-suppress 't)

          (with-silent-modifications
            (mapcar 'pfront-process-single l1))
          (setq pfront-process-buffer "")
          (setq pfront-suppress nil)
          ))))

(defun pfront-process-sentinel (process state)
  "Monitor STATE change of PROCESS."
  )

(defvar pfront-prechange-end 0)

(defun pfront-pre-change (start end)
  "Remember the line"
  (if pfront-lastfailure
              (progn
                (set-text-properties (car pfront-lastfailure)
                                     (cadr pfront-lastfailure)
                                     nil)
                (setq pfront-lastfailure nil)
                ))
  (if (not pfront-suppress)
      (setq pfront-prechange-end (+ 1 (line-number-at-pos end)))))

(defun pfront-post-change (start end length)
  "Act on a buffer change"
  (if (not pfront-suppress)
      (let* ((cl (line-number-at-pos start))
             (el (+ 1 (line-number-at-pos end)))
             (clpos (save-excursion (goto-char start) (line-beginning-position)))
             (elpos (save-excursion (goto-char end) (line-beginning-position 2)))
             (data (buffer-substring-no-properties clpos elpos)))
        (set-text-properties clpos elpos nil)
        (pfront-update-region data (length data) (- cl 1) (- el 1)
                              (- pfront-prechange-end 1))
        )))

;;;###autoload
(defun pfront ()
  "Start Pfront."
  (interactive)
  (let ((buffer (make-comint "pfront"  pfront-command)))
    (setq pfront-comint-buffer buffer)
    (with-current-buffer buffer
      (let ((process (get-buffer-process buffer)))
        (set-process-filter process 'pfront-process-filter)
        (set-process-sentinel process 'pfront-process-sentinel)
        ))
    (pfront-update)
    (remove-hook 'after-change-functions 'pfront-post-change)
    (add-hook 'after-change-functions 'pfront-post-change t t)
    (remove-hook 'before-change-functions 'pfront-pre-change)
    (add-hook 'before-change-functions 'pfront-pre-change t t)

    ))

(defun pfront-update-region (str size fline lastline oldlastline)
  "Reparse a part"
    (setq pfront-processed-buffer (current-buffer))
   (let ((bname (buffer-name)))
    (with-current-buffer pfront-comint-buffer
      (let ((process (get-buffer-process (current-buffer))))
        (comint-send-string process bname)
        (comint-send-string process "\n")
        (comint-send-string process (int-to-string size))
        (comint-send-string process "\n")
        (comint-send-string process (int-to-string fline))
        (comint-send-string process "\n")
        (comint-send-string process "0")
        (comint-send-string process "\n")
        (comint-send-string process (format "%d\n" lastline))
        (comint-send-string process (format "%d\n" oldlastline))
        (comint-send-string process str)
        (comint-send-string process "[:EOB:]\n")
        (comint-send-input)
        ))))


(defun pfront-update ()
  "Reparse buffer"
  (interactive)
  (let ((size (buffer-size))
        (bname (buffer-name))
        (buffer-contents (buffer-string)))
    (setq pfront-processed-buffer (current-buffer))

    (let ((clpos (save-excursion (goto-char 0) (line-beginning-position)))
          (elpos (save-excursion (goto-char size) (line-beginning-position 2))))
      (set-text-properties clpos elpos nil))

    (with-current-buffer pfront-comint-buffer
      (let ((process (get-buffer-process (current-buffer))))
        (comint-send-string process bname)
        (comint-send-string process "\n")
        (comint-send-string process (int-to-string size))
        (comint-send-string process "\n")
        (comint-send-string process "0")
        (comint-send-string process "\n")
        (comint-send-string process "0")
        (comint-send-string process "\n")
        (comint-send-string process "0\n")
        (comint-send-string process "0\n")
        (comint-send-string process buffer-contents)
        (comint-send-string process "[:EOB:]\n")
        (comint-send-input)
        ))))

  (defun pfront-mode-line ())
(defun pfront-quit ()
  "Quit Pfront."
  (interactive)
  (kill-buffer (get-buffer pfront-buffer-name)))

(defvar pfront-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'pfront-do-foo)
    map)
  "Keymap for `pfront-mode'.")


;(defvar pfront-imenu-generic-expression)

; (defvar pfront-outline-regexp)

;;;###autoload
(define-derived-mode pfront-mode fundamental-mode "Pfront"
  "A major mode for editing Pfront files."
  (set (make-local-variable 'indent-line-function) 'pfront-indent-line)
;  (set (make-local-variable 'imenu-generic-expression)
;       pfront-imenu-generic-expression)
;  (set (make-local-variable 'outline-regexp) pfront-outline-regexp)
  (pfront)

  )

;;; Indentation

(defun pfront-indent-line ()
  "Indent current line of Pfront code."
  (interactive)
  (let* ((savep (point))
         (indent (condition-case nil
                     (save-excursion
                       (forward-line 0)
                       (skip-chars-forward " \t")
                       (if (>= (point) savep) (setq savep nil))
                       (max (pfront-calculate-indentation) 0))
                   (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun pfront-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (let* ((ln (line-number-at-pos (point)))
         (ilist (get_prev_indent_list ln)))
    (if ilist (car ilist) 0)))

(provide 'pfront)
;; arch-tag: b5d7a461-b1bc-4f32-b3b7-cad11d95017d
;;; pfront.el ends here
