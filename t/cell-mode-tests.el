;;; cell-mode-tests.el --- ERT suite for cell-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch tests for cell-mode.el, run with `make test'.
;;
;; Three groups:
;;   - the grid data structure (pure vector code, no buffer);
;;   - cells (values, expression cells, sexp->cell);
;;   - a live sheet in a buffer: open, fill, render, serialize, read
;;     back, undo.
;;
;; Known defects are pinned as `:expected-result :failed', so the suite
;; stays green while saying plainly what is broken; when one is fixed,
;; ERT reports it as an unexpected pass and the marker comes off.
;;
;; `cell-mode-tests-prove-red' fails on purpose when the environment
;; variable CELL_MODE_PROVE_RED is set; `make prove-red' uses it to show
;; the suite can go red.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cell-mode)

;;; Text terminals

;; `cell-char-width-pixels' asks the frame's font for a glyph width.  A
;; batch or tty Emacs has no font object, so every cell label would
;; error with (wrong-type-argument font-object nil).  When there is no
;; graphic display, measure in columns instead, as a text terminal does.
;; This is the only stand-in the suite uses; on a graphic display the
;; real function runs.

(defun cell-mode-tests--column-width (char)
  "Width of CHAR in columns, standing in for pixels on a text terminal."
  (char-width char))

(unless (display-graphic-p)
  (advice-add 'cell-char-width-pixels :override
              #'cell-mode-tests--column-width))

;;; Helpers

(defmacro cell-mode-tests--with-sheet (&rest body)
  "Run BODY in a fresh, empty Cell-mode buffer; clean up afterwards."
  (declare (indent 0) (debug t))
  `(let ((buf (generate-new-buffer " *cell-mode-test*")))
     (unwind-protect
         (with-current-buffer buf
           (cell-mode)
           ,@body)
       (cell-stop-timer)
       (when (buffer-live-p buf)
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf)))))

(defun cell-mode-tests--put (row col value)
  "Put an expression cell holding VALUE at ROW, COL of the current sheet."
  (cell-grid-set (cell-sheet-grid cell-current-sheet) row col
                 (cell-sexp->cell value)))

(defun cell-mode-tests--value-at (row col)
  "Return the value of the cell at ROW, COL of the current sheet, or nil."
  (let ((c (cell-grid-get (cell-sheet-grid cell-current-sheet) row col)))
    (and c (cell-get-value c))))

;;; The grid

(ert-deftest cell-mode-tests-grid-shape ()
  (let ((g (cell-make-grid 3 5)))
    (should (= 3 (cell-grid-rows g)))
    (should (= 5 (cell-grid-columns g)))
    (should (null (cell-grid-get g 2 4)))))

(ert-deftest cell-mode-tests-grid-set-get ()
  (let ((g (cell-make-grid 2 2)))
    (cell-grid-set g 1 0 'x)
    (should (eq 'x (cell-grid-get g 1 0)))
    (should (null (cell-grid-get g 0 1)))))

(ert-deftest cell-mode-tests-grid-out-of-range-is-quiet ()
  "Reads and writes past the edge are ignored, not errors."
  (let ((g (cell-make-grid 2 2)))
    (should (null (cell-grid-get g 5 0)))
    (should (null (cell-grid-get g 0 5)))
    (cell-grid-set g 9 9 'x)
    (should (equal g (cell-make-grid 2 2)))))

(ert-deftest cell-mode-tests-vector-insert ()
  (should (equal [a x b c] (cell-vector-insert [a b c] 1 'x)))
  (should (equal [x a] (cell-vector-insert [a] 0 'x)))
  (should (equal [a x] (cell-vector-insert [a] 1 'x))))

(ert-deftest cell-mode-tests-vector-delete ()
  (should (equal [a c] (cell-vector-delete [a b c] 1)))
  (should (equal [b c] (cell-vector-delete [a b c] 0)))
  (should (equal [a b] (cell-vector-delete [a b c] 2))))

(ert-deftest cell-mode-tests-grid-insert-delete-row ()
  (let ((g (cell-make-grid 2 3)))
    (cell-grid-set g 1 2 'z)
    (let ((g2 (cell-grid-insert-row g 0)))
      (should (= 3 (cell-grid-rows g2)))
      (should (= 3 (cell-grid-columns g2)))
      (should (eq 'z (cell-grid-get g2 2 2)))
      (let ((g3 (cell-grid-delete-row g2 0)))
        (should (= 2 (cell-grid-rows g3)))
        (should (eq 'z (cell-grid-get g3 1 2)))))))

(ert-deftest cell-mode-tests-grid-insert-delete-column ()
  (let ((g (cell-make-grid 2 2)))
    (cell-grid-set g 0 1 'y)
    (setq g (cell-grid-insert-column g 0))
    (should (= 3 (cell-grid-columns g)))
    (should (eq 'y (cell-grid-get g 0 2)))
    (setq g (cell-grid-delete-column g 0))
    (should (= 2 (cell-grid-columns g)))
    (should (eq 'y (cell-grid-get g 0 1)))))

;;; Cells

(ert-deftest cell-mode-tests-cell-value ()
  (let ((c (make-instance 'cell)))
    (cell-set-value c 42)
    (should (= 42 (cell-get-value c)))
    (should (= 42 (cell-find-value c)))
    (should (cell-collect-value-p c))))

(ert-deftest cell-mode-tests-expression-cell-reads-and-executes ()
  (let ((c (make-instance 'cell-expression)))
    (cell-accept-string-value c "(+ 1 2)")
    (should (equal "(+ 1 2)" (cell-label c)))
    (should (equal '(+ 1 2) (cell-find-value c)))
    (should (= 3 (cell-execute c)))))

(ert-deftest cell-mode-tests-comment-cell-face ()
  (let ((c (make-instance 'cell-comment-cell)))
    (cell-set-value c "a note")
    (should (eq 'cell-comment-2-face (cell-face c)))))

(ert-deftest cell-mode-tests-sexp->cell-string ()
  (let ((s (cell-sexp->cell "hello")))
    (should (cl-typep s 'cell-expression))
    (should (equal "hello" (cell-label s)))
    (should (equal "hello" (cell-get-value s)))))

;;; A live sheet

(ert-deftest cell-mode-tests-sheet-opens-blank ()
  (cell-mode-tests--with-sheet
    (should (eq major-mode 'cell-mode))
    (should (cl-typep cell-current-sheet 'cell-sheet))
    (should (equal '(0 0) (cell-sheet-cursor cell-current-sheet)))
    (should buffer-read-only)
    (should-not (buffer-modified-p))))

(ert-deftest cell-mode-tests-sheet-renders-labels ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 1 2 "zebra")
    (cell-sheet-update)
    (should (string-match-p
             "zebra" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest cell-mode-tests-collect-cells-by-row ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "a")
    (cell-mode-tests--put 0 3 "b")
    (cell-mode-tests--put 2 1 "c")
    (should (equal '(("a" "b") ("c"))
                   (mapcar (lambda (row) (mapcar #'cell-get-value row))
                           (cell-collect-cells-by-row cell-current-sheet))))))

(ert-deftest cell-mode-tests-serialize-round-trip ()
  "A sheet written by EIEIO-PERSISTENT reads back with its cells."
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "north")
    (cell-mode-tests--put 3 2 "(+ 2 2)")
    (let* ((text (cell-sheet-serialize-eieio cell-current-sheet))
           (back (cell-sheet-read (car (read-from-string text)))))
      (setq cell-current-sheet back)
      (should (equal "north" (cell-mode-tests--value-at 0 0)))
      (should (equal "(+ 2 2)" (cell-mode-tests--value-at 3 2)))
      (should (null (cell-mode-tests--value-at 1 1))))))

(ert-deftest cell-mode-tests-undo-restores-previous-sheet ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "before")
    (cell-push-undo-history)
    (cell-mode-tests--put 0 0 "after")
    (should (equal "after" (cell-mode-tests--value-at 0 0)))
    (let ((inhibit-message t))
      (cell-pop-undo-history))
    (should (equal "before" (cell-mode-tests--value-at 0 0)))))

;;; Commands, as a user drives them

(defun cell-mode-tests--goto (row col)
  "Put the sheet's cursor at ROW, COL."
  (setf (cell-sheet-cursor cell-current-sheet) (list row col)))

(defmacro cell-mode-tests--answering (answer &rest body)
  "Run BODY with every minibuffer prompt answered by ANSWER."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'read-from-minibuffer)
              (lambda (&rest _) ,answer))
             ((symbol-function 'read-file-name)
              (lambda (&rest _) ,answer)))
     (let ((inhibit-message t)) ,@body)))

(ert-deftest cell-mode-tests-cmd-create-and-edit-cell ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--goto 2 3)
    (cell-mode-tests--answering "(* 6 7)" (cell-sheet-create-cell))
    (should (equal "(* 6 7)" (cell-mode-tests--value-at 2 3)))
    (cell-mode-tests--answering "(* 2 2)" (cell-sheet-create-cell))
    (should (equal "(* 2 2)" (cell-mode-tests--value-at 2 3)))
    (should (= 4 (cell-sheet-execute)))))

(ert-deftest cell-mode-tests-cmd-empty-answer-makes-no-cell ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--answering "" (cell-sheet-create-cell))
    (should (null (cell-grid-get (cell-sheet-grid cell-current-sheet) 0 0)))))

(ert-deftest cell-mode-tests-cmd-create-comment ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--goto 1 1)
    (cell-mode-tests--answering "a remark" (cell-sheet-create-comment))
    (should (cl-typep (cell-grid-get (cell-sheet-grid cell-current-sheet) 1 1)
                      'cell-comment-cell))))

(ert-deftest cell-mode-tests-cmd-delete-cell ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "gone")
    (cell-sheet-delete-cell)
    (should (null (cell-mode-tests--value-at 0 0)))))

(ert-deftest cell-mode-tests-cmd-insert-and-delete-row ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 1 0 "row1")
    (cell-mode-tests--goto 0 0)
    (cell-sheet-insert-row)
    (should (equal "row1" (cell-mode-tests--value-at 2 0)))
    (cell-sheet-delete-row)
    (should (equal "row1" (cell-mode-tests--value-at 1 0)))))

(ert-deftest cell-mode-tests-cmd-insert-and-delete-column ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 1 "col1")
    (let ((cols (cell-grid-columns (cell-sheet-grid cell-current-sheet))))
      (cell-mode-tests--goto 0 0)
      (cell-sheet-insert-column)
      (should (= (1+ cols) (cell-grid-columns (cell-sheet-grid cell-current-sheet))))
      (should (equal "col1" (cell-mode-tests--value-at 0 2)))
      (cell-sheet-delete-column)
      (should (= cols (cell-grid-columns (cell-sheet-grid cell-current-sheet))))
      (should (equal "col1" (cell-mode-tests--value-at 0 1))))))

(ert-deftest cell-mode-tests-cmd-copy-paste-one-cell ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "twin")
    (let ((inhibit-message t)) (cell-sheet-copy-to-clipboard))
    (cell-mode-tests--goto 4 4)
    (cell-sheet-paste)
    (should (equal "twin" (cell-mode-tests--value-at 4 4)))
    (should (equal "twin" (cell-mode-tests--value-at 0 0)))
    (should-not (eq (cell-grid-get (cell-sheet-grid cell-current-sheet) 0 0)
                    (cell-grid-get (cell-sheet-grid cell-current-sheet) 4 4)))))

(ert-deftest cell-mode-tests-cmd-copy-paste-selection ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "a")
    (cell-mode-tests--put 0 1 "b")
    (cell-mode-tests--put 1 1 "d")
    (cell-mode-tests--goto 0 0)
    (cell-sheet-set-mark)
    (cell-mode-tests--goto 1 1)
    (cell-sheet-update-selection-from-mark)
    (let ((inhibit-message t)) (cell-sheet-copy-to-clipboard))
    (should (null (cell-sheet-selection cell-current-sheet)))
    (cell-mode-tests--goto 5 5)
    (cell-sheet-paste)
    (should (equal "a" (cell-mode-tests--value-at 5 5)))
    (should (equal "b" (cell-mode-tests--value-at 5 6)))
    (should (null (cell-mode-tests--value-at 6 5)))
    (should (equal "d" (cell-mode-tests--value-at 6 6)))))

(ert-deftest cell-mode-tests-cmd-cut-paste-selection ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "p")
    (cell-mode-tests--put 0 1 "q")
    (cell-sheet-set-mark)
    (cell-mode-tests--goto 0 1)
    (cell-sheet-update-selection-from-mark)
    (let ((inhibit-message t)) (cell-sheet-cut-to-clipboard))
    (should (null (cell-mode-tests--value-at 0 0)))
    (should (null (cell-mode-tests--value-at 0 1)))
    (cell-mode-tests--goto 2 0)
    (cell-sheet-paste)
    (should (equal "p" (cell-mode-tests--value-at 2 0)))
    (should (equal "q" (cell-mode-tests--value-at 2 1)))))

(ert-deftest cell-mode-tests-known-cut-one-cell ()
  "Known defect: cutting with no selection loses the cell.
With no selection, `cell-sheet-cut-to-clipboard' blanks the cell at
the cursor without first copying it, so a paste brings back nothing."
  :expected-result :failed
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "moving")
    (let ((inhibit-message t)) (cell-sheet-cut-to-clipboard))
    (should (null (cell-mode-tests--value-at 0 0)))
    (cell-mode-tests--goto 3 3)
    (cell-sheet-paste)
    (should (equal "moving" (cell-mode-tests--value-at 3 3)))))

(ert-deftest cell-mode-tests-cmd-undo-then-redo-a-paste ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "x")
    (let ((inhibit-message t)) (cell-sheet-copy-to-clipboard))
    (cell-mode-tests--goto 2 2)
    (cell-sheet-paste)
    (let ((inhibit-message t)) (cell-sheet-undo))
    (should (null (cell-mode-tests--value-at 2 2)))
    (should (equal "x" (cell-mode-tests--value-at 0 0)))
    (let ((inhibit-message t)) (cell-sheet-redo))
    (should (equal "x" (cell-mode-tests--value-at 2 2)))))

(ert-deftest cell-mode-tests-cmd-undo-keeps-cursor ()
  (cell-mode-tests--with-sheet
    (cell-mode-tests--goto 3 1)
    (cell-mode-tests--answering "kept" (cell-sheet-create-cell))
    (let ((inhibit-message t)) (cell-sheet-undo))
    (should (equal '(3 1) (cell-sheet-cursor cell-current-sheet)))
    (should (null (cell-mode-tests--value-at 3 1)))))

(ert-deftest cell-mode-tests-cmd-undo-with-empty-history-is-harmless ()
  (cell-mode-tests--with-sheet
    (cl-letf (((symbol-function 'beep) #'ignore))
      (let ((inhibit-message t))
        (cell-pop-undo-history)
        (cell-sheet-redo)))
    (should (cl-typep cell-current-sheet 'cell-sheet))))

(ert-deftest cell-mode-tests-cmd-cursor-moves-and-stays-in-bounds ()
  (cell-mode-tests--with-sheet
    (cell-sheet-move-cursor-down)
    (cell-sheet-move-cursor-right)
    (should (equal '(1 1) (cell-sheet-cursor cell-current-sheet)))
    (cell-sheet-move-cursor-up)
    (cell-sheet-move-cursor-up)
    (cell-sheet-move-cursor-left)
    (cell-sheet-move-cursor-left)
    (should (equal '(0 0) (cell-sheet-cursor cell-current-sheet)))))

(ert-deftest cell-mode-tests-file-save-and-visit ()
  "A sheet saved to a file opens again with its cells."
  (let ((file (make-temp-file "cell-mode-test" nil ".cell")))
    (unwind-protect
        (progn
          (let ((buf (find-file-noselect file)))
            (with-current-buffer buf
              (cell-mode)
              (cell-mode-tests--put 1 1 "kept on disk")
              (cell-sheet-update)
              (let ((inhibit-message t)) (save-buffer))
              (cell-stop-timer)
              (kill-buffer buf)))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (let ((inhibit-message t)) (cell-mode))
                  (should (equal "kept on disk" (cell-mode-tests--value-at 1 1))))
              (cell-stop-timer)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-file file))))

;;; Known defects (pinned, not fixed here)

(ert-deftest cell-mode-tests-setting-value ()
  "A setting cell holding [NAME VALUE] is found by name.
Was a defect: `cell-sheet-collect-settings' bound `cl-values' but
looped over the unrelated global `values'."
  (cell-mode-tests--with-sheet
    (cell-mode-tests--put 0 0 "[cell-mode-tests-setting 9]")
    (should (= 9 (cell-sheet-setting-value cell-current-sheet
                                           'cell-mode-tests-setting)))))

(ert-deftest cell-mode-tests-comment-cell-not-collected ()
  "Comment cells are labels, so their values are not collected.
Was a defect: the method had a docstring and no body, so it returned
the docstring, a non-nil string."
  (let ((c (make-instance 'cell-comment-cell)))
    (cell-set-value c "a note")
    (should-not (cell-collect-value-p c))))

(ert-deftest cell-mode-tests-sexp->cell-non-string ()
  "`cell-sexp->cell' keeps a non-string value and labels it in print.
Was a defect: the expression cell's :after method measured the raw
value as a string, (wrong-type-argument sequencep 7)."
  (let ((n (cell-sexp->cell 7)))
    (should (equal "7" (cell-label n)))
    (should (= 7 (cell-get-value n)))))

;;; The suite can fail

(ert-deftest cell-mode-tests-prove-red ()
  "Fail on purpose when CELL_MODE_PROVE_RED is set (see `make prove-red')."
  (skip-unless (getenv "CELL_MODE_PROVE_RED"))
  (should (equal "planted" "failure")))

(provide 'cell-mode-tests)

;; Copyright (C) 2026 Corwin Brust <corwin@bru.st>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; cell-mode-tests.el ends here
