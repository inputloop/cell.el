;;; cell-mode.el --- EIEIO spreadsheet mode -*- lexical-binding: nil; -*-

;; Copyright (C) 2006-2007, 2019-2021 by David O'Toole

;; Author: David O'Toole <deeteeoh1138@gmail.com>
;; Maintainer: David O'Toole <deeteeoh1138@gmail.com>
;; Package-Requires: ((emacs "25.1"))
;; URL: https://gitlab.com/dto/cell-mode
;; License: GPLv3
;; Version: 2.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; (Version 3) as published by the Free Software Foundation, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program, in a text file called LICENSE. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Summary:

;; Cell-mode implements an abstract spreadsheet control in an Emacs
;; buffer.  The purpose of Cell-mode is to provide a major mode for
;; spreadsheet-based user interfaces; it can be further extended by
;; defining application-specific Emacs Lisp minor modes which supply
;; new cell and spreadsheet classes via Emacs' included object system,
;; EIEIO. For a video demo, see https://emacsconf.org/2019/talks/18/

;; Features:

;; - Uses a simple file format based on EIEIO-PERSISTENT.  Cell-mode
;;   files can be visited, saved, loaded, and so on just like any other
;;   text file.  Works with Emacs' existing autosave, version control,
;;   backup, and TRAMP features.
;; - You can cut, copy, and paste cells between sheets using typical
;;   Emacs shortcuts, as well as insert and delete rows and
;;   columns. (CUA-mode not yet supported.)
;; - Undo/redo support.
;; - Execute Lisp expressions in cells, and other user defined actions.
;; - Rectangle selection via keyboard and mouse-drag are available.
;; - Object-orientation through the use of Emacs' included object
;;   system, EIEIO.  Cells can contain any Lisp value and respond
;;   polymorphically to events.
;; - Contents can be collected and processed however you want.
;; - Cells can display images, Unicode text, and variable-width fonts.

;; Issues:

;; - Cursor can display incorrectly in cells with Unicode characters
;;   or images.
;; - Needs more optimization.
;; - No data dependency resolution; this will be implemented.
;; - Changing font size can cause incorrect rendering; try
;;   reinitializing cell-mode with M-x cell-mode RET.

;; Notes:

;; - It is highly recommended to byte-compile this file so that
;;   commands execute quickly.
;; - Cell-mode source dates originally to 2006 and thus uses dynamic
;;   binding by default.  Two warnings about free variables named
;;   "row" and "col" are to be expected during byte-compilation.

;; Getting started:

;; Download cell.el and place it in your Emacs load-path.  Then:

;;  (require 'cell)
;;  (add-to-list 'auto-mode-alist (cons "\\.cell" 'cell-mode))

;; Then use M-x CELL-SHEET-CREATE, or visit a new file with the
;; extension ".cell".  Press "Control-H m" to get help on the
;; keybindings.

;; To byte-compile and load cell-mode in one step, use:

;;   (byte-compile-file "/path/to/cell.el" t)

;; How to extend Cell-mode:

;; - Use EIEIO's DEFCLASS to derive new subclasses from the included
;;   CELL and CELL-SHEET classes in order to implement the new
;;   behaviors you want; use DEFINE-DERIVED-MODE or DEFINE-MINOR-MODE
;;   to extend the Emacs mode if necessary.  Available CELL methods
;;   are:

;;     (INITIALIZE-INSTANCE :AFTER (C CELL) &KEY)
;;       Define this :AFTER method for any initialization you need.

;;     (CELL-EXECUTE (C CELL))
;;       Perform a cell's action.  Default S-Expression cells are evaluated.

;;     (CELL-COLLECT-VALUE-P (C CELL))
;;       Return non-nil if CELL-COLLECT-CELLS and related functions should
;;       include this cell in their output.

;;     (CELL-GET-VALUE (C CELL))
;;       Return the stored value.

;;     (CELL-SET-VALUE (C CELL) VALUE)
;;       Set the value.  You can define :BEFORE, :AFTER methods here
;;       etc.

;;     (CELL-FIND-VALUE (C CELL))
;;       Lazily find the value.  Just returns the value by default;
;;       subclasses can use this for cache-on-demand behavior.

;;     (CELL-TAP (C CELL))
;;       Primary (left-click) action for cells.  By default this just
;;       selects the cell.

;;     (CELL-ALTERNATE-TAP (C CELL))
;;       Secondary click action for cells.  By default this calls
;;       CELL-EXECUTE.

;;     (CELL-ACCEPT-STRING-VALUE (C CELL) STRING)
;;       Accept a string representing the value, instead of the value.
;;       Normally you will not have to modify this.

;;  Other functions include:

;;      (CELL-SHEET-CREATE &OPTIONAL ROWS COLS CLASS)
;;        Create a new cell sheet in a new buffer with ROWS rows and
;;        COLS columns.  Optionally use CLASS instead of the default
;;        cell sheet class.

;;      (CELL-COLLECT-CELLS SHEET)
;;        Return a flat list of all cells in the spreadsheet, in
;;        left-to-right then top-to-bottom order.

;;      (CELL-COLLECT-CELLS-BY-ROW SHEET)
;;        Return a list of rows of cells from the spreadsheet.

;;   The buffer-local variable CELL-CURRENT-SHEET is always set to the
;;   current CELL-CURRENT-SHEET object in Cell-mode buffers.

;;   Documentation for the CELL-SHEET class is forthcoming.  Read on
;;   and check the docstrings below if you wish to learn more.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'cl))
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(require 'eieio-speedbar)

;;; EIEIO compatibility macros

;; The following compatibility macros transform code using
;; DEFSTRUCT-style class definitions and slot accessors into
;; EIEIO. This is transparent to the cell and sheet subclasses you
;; define, which can just use the normal EIEIO definitions and
;; accessors.

(cl-defmacro cell-defslot (class slot)
  "Define for CLASS the slot SLOT, emulating legacy DEFSTRUCT slots."
  (let ((instance (cl-gensym))
        (value (cl-gensym))
        (getter (intern (concat (symbol-name class) "-" (symbol-name slot))))
        (setter (intern (concat "cell-set-value/"
                                (symbol-name class) "-" (symbol-name slot)))))
    `(progn
       (defun ,getter (,instance)
         (slot-value ,instance ',slot))
       (defun ,setter (,instance ,value)
         (setf (slot-value ,instance ',slot)
               ,value))
       (defsetf ,getter ,setter))))

(cl-defmacro cell-defclass (name superclasses &rest specs)
  "Define a class called NAME with superclasses SUPERCLASSES.
The list SPECS is the legacy slot definition."
  `(defclass ,name ,superclasses
     ,(mapcar #'(lambda (spec)
                  (if (listp spec)
                      spec
                    (list
                     spec
                     :initform nil
                     :initarg (intern (concat ":" (symbol-name spec))))))
              specs)))

;;; Customization

(defcustom cell-cursor-blink-interval 0.5
  "How many seconds to wait between cursor blinks."
  :tag "Cursor blink interval"
  :group 'cell
  :type 'number)

(defcustom cell-cursor-blink-p t
  "When non-nil, blink active cursor."
  :group 'cell
  :type 'boolean)

(defcustom cell-blank-width 4 "Default width of blank cells, in spaces."
  :group 'cell
  :type 'integer)

(defcustom cell-no-label-string "no label"
  "Default string for when a cell has no label."
  :group 'cell
  :type 'string)

;;; Utilities

(defun cell-char-width-pixels (char)
  "Return the width in pixels of the character CHAR."
  (with-temp-buffer
    (set-window-buffer (selected-window) (current-buffer))
    (insert-char char)
    (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4)))

(defun cell-string-width (str)
  "Return the width of the string STR."
  (/ (float (apply #'+ (cl-map 'list #'cell-char-width-pixels str)))
     (float (window-font-width))))

;;; Variables

(defvar cell-timer nil "Timer object for periodic updates.")

(defvar cell-current-sheet nil "This buffer's cell-sheet object.")
(make-variable-buffer-local 'cell-sheet)

(defvar cell-clipboard nil "List of cell rows on the clipboard.")

(defvar cell-cached-buffer-positions nil)
(make-variable-buffer-local 'cell-cached-buffer-positions)

(defvar cell-cached-buffer-positions* nil)
(make-variable-buffer-local 'cell-cached-buffer-positions*)

(cell-defclass cell (eieio-persistent)
               label       ; label to be displayed on the cell
               label-width ; width of label (when not integral; used for images)
               value       ; can be any lisp object the cell requires.
               face        ; face to display cell label with,
                           ; or use propertized text for label)
               )

(cell-defslot cell label)
(cell-defslot cell label-width)
(cell-defslot cell value)
(cell-defslot cell face)

(cl-defmethod initialize-instance :after ((c cell) &rest slots)
  "Initialize the cell C with slot specificiations SLOTS."
  nil)

(cl-defmethod cell-execute ((c cell))
  "Perform action when cell C is executed."
  nil)

(cl-defmethod cell-collect-value-p ((c cell))
  "When non-nil, collect the value of the cell C.
This occurs during CELL-COLLECT-CELLS and related calls."
  t)

(cl-defmethod cell-get-value ((c cell))
  "Return the value of the cell C."
  (slot-value c 'value))

(cl-defmethod cell-set-value ((c cell) value)
  "Set the value of cell C to VALUE."
  (setf (slot-value c 'value) value))

(cl-defmethod cell-find-value ((c cell))
  "Lazily find the value of cell C.
Subclasses can use this for caching."
  (cell-get-value c))

(cl-defmethod cell-accept-string-value ((c cell) string)
  "For cell C, accept a STRING instead of a raw value."
  (cell-set-value c string))

(cl-defmethod cell-tap ((c cell))
  "Primary click action for cell C." nil)

(cl-defmethod cell-alternate-tap ((c cell))
  "Secondary click action for cell C.
By default this executes the cell."
  (cell-execute c))

(cl-defmethod cell-contains-image-p ((c cell))
  "Return non-nil if the cell C has an image."
  nil)

;;; Blinking the cursor and other periodic updates

(defun cell-update-cursor-blink ()
  "Update the cursor's blinking activity."
  (when (and cell-cursor-blink-p
             (eq major-mode 'cell-mode)
             (eq (current-buffer) (window-buffer)))
    (cell-sheet-display-cursors (overlays-in (point-min) (point-max)))))

(defun cell-start-timer ()
  "Start the cursor blink timer."
  (when (timerp cell-timer)
    (cancel-timer cell-timer))
  (setf cell-timer (run-with-timer
                    0 cell-cursor-blink-interval
                    'cell-update-cursor-blink)))

(defun cell-stop-timer ()
  "Stop the cursor blink timer."
  (when (timerp cell-timer)
    (cancel-timer cell-timer)))

(cl-defmethod cell-fill-space ((cell cell) face)
  "Fill the rest of the space for CELL with FACE."
  nil)

;;; Lisp expression cells

(cell-defclass cell-expression (cell))

(cl-defmethod cell-set-value :after ((c cell-expression) value)
  "Update the formatting of the expression cell C with VALUE."
  (setf (cell-label-width c)
        (cell-string-width value))
  (setf (cell-label c) value))

(cl-defmethod cell-find-value ((exp cell-expression))
  "Read the expression string contained in the cell EXP."
  (when (stringp (cell-value exp))
    (car (read-from-string (cell-value exp)))))
  
(cl-defmethod cell-execute ((exp cell-expression))
  "Evaluate the sexp in the cell EXP."
  (eval (cell-find-value exp)))

(cl-defmethod cell-fill-space ((exp cell-expression) face)
  "Fill the rest of the space for cell EXP with FACE."
  (let ((width (cell-label-width exp)))
    (when width
      (cell-insert-spacer (ceiling width) face))))

;;; Comment cells

(cell-defclass cell-comment-cell (cell-expression))

(cl-defmethod cell-collect-value-p ((c cell-comment-cell))
  "Return non-nil if the value of cell C is to be collected.")

(cl-defmethod cell-set-value :after ((c cell-comment-cell) value)
  "Set the face of the comment cell C after setting VALUE."
  (setf (cell-face c) 'cell-comment-2-face))

;;; Image cells

(cell-defclass cell-image-cell (cell) image)
(cell-defslot cell image)

(cl-defmethod cell-set-value :after ((i cell-image-cell) image-name)
  "Set image test properties for the image cell I after setting IMAGE-NAME."
  (setf (cell-image i)
        (create-image (car (read-from-string image-name))))
  (setf (cell-label-width i)
        (car (image-size (cell-image i))))
  (setf (cell-label i)
        (propertize "  " 'display (cell-image i))))

(cl-defmethod cell-contains-image-p ((c cell-image-cell))
  "Return non-nil when the cell C has an image."
  t)

;;; Internal grid data structure

(defun cell-make-grid (rows cols)
  "Make a new grid with ROWS rows and COLS columns."
  (let ((grid (make-vector rows nil)))
    (dotimes (row rows)
      (setf (aref grid row) (make-vector cols nil)))
    grid))

(defun cell-grid-get (grid row col)
  "Return the object in GRID at row ROW and column COL."
  (when (and (< row (length grid))
             (< col (length (aref grid 0))))
    (aref (aref grid row) col)))

(defun cell-grid-set (grid row col value)
  "Set the object at GRID location ROW,COL to VALUE."
  (when (and (< row (length grid))
             (< col (length (aref grid 0))))
    (let ((row (aref grid row)))
      (setf (aref row col) value))))

(defun cell-grid-columns (grid)
  "Return the number of columns in the GRID."
  (length (aref grid 0)))

(defun cell-grid-rows (grid)
  "Return the number of rows in the GRID."
  (length grid))

(defun cell-vector-insert (oldvec pos elt)
  "Insert into OLDVEC at position POS the element ELT."
  (let* ((len (length oldvec))
         (newvec (make-vector (+ len 1) nil)))
    (dotimes (i (+ 1 len))
      (setf (aref newvec i) (cond
                             (( < i pos)
                              (aref oldvec i))
                             (( equal i pos)
                              elt)
                             (( > i pos)
                              (aref oldvec (- i 1))))))
    newvec))

(defun cell-vector-delete (oldvec pos)
  "Remove from vector OLDVEC the position POS."
  (let* ((len (length oldvec))
         (newvec (make-vector (- len 1) nil)))
    (dotimes (i (- len 1))
      (setf (aref newvec i) (cond
                             (( < i pos)
                              (aref oldvec i))
                             (( >= i pos)
                              (aref oldvec (+ i 1))))))
    newvec))

(defun cell-grid-insert-row (grid row)
  "Return a copy of GRID with a row inserted at row ROW."
  (let* ((newrow (make-vector (cell-grid-columns grid) nil)))
    (cell-vector-insert grid row newrow)))

(defun cell-grid-insert-column (grid col)
  "Return a copy of GRID with a column inserted at column COL."
  (dotimes (i (cell-grid-rows grid))
    (setf (aref grid i) (cell-vector-insert (aref grid i) col nil)))
  grid)

(defun cell-grid-delete-row (grid row)
  "Return a copy of GRID with the row ROW removed."
  (cell-vector-delete grid row))

(defun cell-grid-delete-column (grid col)
  "Return a copy of GRID with the column COL removed."
  (dotimes (i (cell-grid-rows grid))
    (setf (aref grid i) (cell-vector-delete (aref grid i) col)))
  grid)

;;; Sheets

(cell-defclass cell-sheet (eieio-persistent)
               name ; string title, currently ignored
               rows ; initial requested number of rows
               cols ; initial requested number of columns
               mark ; nil when there is no mark, or (LIST ROW COL)
               (rendering) ; cached buffer text of spreadsheet
               widths ; column widths
               row-header-width ; width of leftmost column
               (buffer) ; what buffer is this sheet in?
               (cursor) ; what cell is the user pointing at? (LIST ROW COL)
               (selection) ; rectangle in this form: (row-1 col-1 row-2 col-2)
               grid ; two-dimensional array of spreadsheet cells
               column-stops ; vector of integers where v[x] is first column number of column x
               borders-p ; whether to display borders
               headers-p ; whether to display headers
               raw-display-p ; whether we are doing raw display
               properties) ; general-purpose plist for extensions

(cell-defslot cell-sheet name)
(cell-defslot cell-sheet rendering)
(cell-defslot cell-sheet mark)
(cell-defslot cell-sheet widths)
(cell-defslot cell-sheet row-header-width)
(cell-defslot cell-sheet rows)
(cell-defslot cell-sheet cols)
(cell-defslot cell-sheet buffer)
(cell-defslot cell-sheet cursor)
(cell-defslot cell-sheet selection)
(cell-defslot cell-sheet grid)
(cell-defslot cell-sheet column-stops)
(cell-defslot cell-sheet borders-p)
(cell-defslot cell-sheet headers-p)
(cell-defslot cell-sheet raw-display-p)
(cell-defslot cell-sheet properties)

(defmacro cell-with-current-cell-sheet (&rest body)
  "Evaluate BODY forms with cell-sheet variables bound."
  (declare (debug t))
  `(let* ((inhibit-read-only t)
          (rendering (cell-sheet-rendering cell-current-sheet))
          (grid (cell-sheet-grid cell-current-sheet))
          (cursor (cell-sheet-cursor cell-current-sheet))
          (cursor-row (cl-first cursor))
          (cursor-column (cl-second cursor))
          (selection (cell-sheet-selection cell-current-sheet))
          (cell^ (cell-grid-get grid cursor-row cursor-column))
          (stops (cell-sheet-column-stops cell-current-sheet)))
     ,@body))

(defun cell-sheet-read (forms)
  "Read the persistent cell sheet from FORMS."
  (let ((sheet (eieio-persistent-make-instance 'cell-sheet (cdr forms))))
    (prog1 sheet)))

;;; Undo/redo support

(defvar cell-undo-history nil "Undo history for current cell sheet.")
(make-variable-buffer-local 'cell-undo-history)

(defvar cell-redo-history nil "Redo history for current cell sheet.")
(make-variable-buffer-local 'cell-redo-history)

(defun cell-push-undo-history ()
  "Push a serialized copy of the cell sheet onto the undo history."
  (push (cell-sheet-serialize-eieio cell-current-sheet)
        cell-undo-history))

(defun cell-push-redo-history ()
  "Push a serialized copy of the cell sheet onto the redo history."
  (push (cell-sheet-serialize-eieio cell-current-sheet)
        cell-redo-history))

(defun cell-pop-undo-history ()
  "Undo the last action."
  (let ((inhibit-read-only t)
        (cursor (cell-sheet-cursor cell-current-sheet)))
    (if (null cell-undo-history)
        (progn (beep) (message "Cell-mode: undo history is empty."))
      (progn
        (setf cell-current-sheet
              (cell-sheet-read
               (car (read-from-string (pop cell-undo-history)))))
        (cell-sheet-update)
        (setf (cell-sheet-cursor cell-current-sheet) cursor)
        (cell-sheet-after-open-hook cell-current-sheet)
        (setf (buffer-modified-p (current-buffer)) t)
        (message "Undo! %d change(s) remaining." (length cell-undo-history))))))

(defun cell-sheet-undo ()
  "Undo the last edit."
  (interactive)
  (cell-with-current-cell-sheet
   (let ((inhibit-read-only t))
     (cell-push-redo-history)
     (cell-pop-undo-history)
     (cell-sheet-update))))

(defun cell-sheet-redo ()
  "Redo the last undo operation."
  (interactive)
  (let ((inhibit-read-only t)
        (cursor (cell-sheet-cursor cell-current-sheet)))
    (if (null cell-redo-history)
        (progn (beep) (message "Cell-mode: redo history is empty."))
      (progn
        (push (pop cell-redo-history)
              cell-undo-history)
        (cell-pop-undo-history)
        (setf (cell-sheet-cursor cell-current-sheet) cursor)
        (cell-sheet-update)
        (message "Redo! %d redo(s) remaining." (length cell-redo-history))))))

;;; Major mode declaration

(defvar cell-mode-map nil "Keymap for Cell-mode.")

(defun cell-find-cell-mode-map ()
  "Return the cell-mode keymap."
  (or cell-mode-map
      (setf cell-mode-map
            (make-sparse-keymap))))

(defun cell-mode-insinuate ()
  "Install local keybindings for Cell-mode."
  (interactive)
  (mapc (lambda (mapping)
            (define-key (cell-find-cell-mode-map) (car mapping) (cdr mapping)))
          `(([(control c)(control e)] . cell-sheet-execute)
            ([(cl-return)] . cell-sheet-create-cell)
            ([(next)] . cell-sheet-page-down)
            ([(prior)] . cell-sheet-page-up)
            ([(control ? )] . cell-sheet-set-mark)
            ([(escape)] . cell-sheet-clear-mark)
            ([(shift return)] . cell-sheet-execute)
            ([(control c)(control c)] . cell-sheet-create-comment)
            ([(control c) ?i] . cell-sheet-create-image)
            ([(control d)] . cell-sheet-delete-cell)
            ([(control z)] . cell-sheet-undo)
            ([(control /)] . cell-sheet-undo)
            ([(control shift z)] . cell-sheet-redo)
            ([(control ??)] . cell-sheet-redo)
            (,(kbd ":") . cell-sheet-create-comment)
            (,(kbd ";") . cell-sheet-create-comment)
            ;; navigating to corners
            ([(meta ?<)] . cell-sheet-move-bob)
            ([(meta ?>)] . cell-sheet-move-eob)
            ;; adding and removing rows
            (,(kbd "\C-c\C-n\C-r") . cell-sheet-insert-row)
            (,(kbd "\C-c\C-n\C-c") . cell-sheet-insert-column)
            (,(kbd "\C-c\C-d\C-r") . cell-sheet-delete-row)
            (,(kbd "\C-c\C-d\C-c") . cell-sheet-delete-column)
            ;; arrow keys
            ([(up)] . cell-sheet-move-cursor-up)
            ([(down)] . cell-sheet-move-cursor-down)
            ([(left)] . cell-sheet-move-cursor-left)
            ([(right)] . cell-sheet-move-cursor-right)
            ;; traditional cursor-motion keys
            ([(control a)] . cell-sheet-move-bol)
            ([(control e)] . cell-sheet-move-eol)
            ([(home)] . cell-sheet-move-bol)
            ([(end)] . cell-sheet-move-eol)
            ([(control f)] . cell-sheet-move-cursor-right)
            ([(control b)] . cell-sheet-move-cursor-left)
            ([(control n)] . cell-sheet-move-cursor-down)
            ([(control p)] . cell-sheet-move-cursor-up)
            ;; the mouse
            ([(mouse-1)] . cell-sheet-mouse-move-cursor)
            ([(mouse-3)] . menu-bar-open)
            ([(mouse-5)] . cell-sheet-page-down)
            ([(mouse-4)] . cell-sheet-page-up)
            ([(drag-mouse-1)] . cell-sheet-mouse-select)
            ;; cut and paste
            ([(meta w)] . cell-sheet-copy-to-clipboard)
            ([(control w)] . cell-sheet-cut-to-clipboard)
            ([(control y)] . cell-sheet-paste)))
    (cell-start-timer))

(cell-mode-insinuate)

(define-derived-mode cell-mode nil "Cell"
  "Abstract spreadsheet display mode."
  (let ((inhibit-read-only t))
    (make-local-variable 'cell-sheet)
    (make-local-variable 'tool-bar-map)
    (setf tool-bar-map (cell-enable-tool-bar-map))
    (let ((fulltext (buffer-substring-no-properties (point-min) (point-max))))
      (if (zerop (length fulltext))
          ;; new blank sheet
          (setf cell-current-sheet
                (make-instance 'cell-sheet :name (buffer-file-name (current-buffer))))
        ;; recover serialized sheet
        (setf cell-current-sheet (cell-sheet-read (car (read-from-string fulltext)))))
      (cl-assert (eieio-object-class cell-current-sheet))
      (message "Loaded cell sheet with class: %S" (eieio-object-class cell-current-sheet))
      ;; finish initializing cell mode in new buffer
      (setf cell-cached-buffer-positions (make-hash-table :test 'equal))
      (setf cell-cached-buffer-positions* (make-hash-table :test 'equal))
      (delete-region (point-min) (point-max))
      (add-hook 'before-save-hook 'cell-remove-rendering)
      (add-hook 'after-save-hook 'cell-sheet-update*)
      (cell-mode-insinuate)
      (cell-sheet-update)
      (cell-sheet-after-open-hook cell-current-sheet)
      (setq-local show-trailing-whitespace nil)
      (setf (buffer-modified-p (current-buffer)) nil))))

;;; Initializing sheets

(cl-defmethod initialize-instance :after ((sheet^ cell-sheet) &rest slots)
  "Initialize SHEET^ and its buffer for Cell-mode with init slots SLOTS."
  (setf cell-current-sheet sheet^)
      ;; fix up any unbound slots
  (when (not (slot-boundp sheet^ 'rendering))
    (setf (slot-value sheet^ 'rendering) nil))
  (when (not (slot-boundp sheet^ 'buffer))
    (setf (slot-value sheet^ 'buffer) (current-buffer)))
  (when (not (slot-boundp sheet^ 'cursor))
    (setf (slot-value sheet^ 'cursor) '(0 0)))
  (when (not (slot-boundp sheet^ 'selection))
    (setf (slot-value sheet^ 'selection) nil))
  (let* ((new-buffer (current-buffer))
         (inhibit-read-only t)
         (rows (or (slot-value sheet^ 'rows) 24))
         (cols (or (slot-value sheet^ 'cols) 16)))
    (with-slots (buffer grid cursor column-stops
                        borders-p headers-p)
        cell-current-sheet
      (setf buffer new-buffer)
      (setf grid (or grid (cell-make-grid rows cols)))
      (setf column-stops (make-vector (+ cols 1) 0))
      (setf cursor '(0 0))
      (setf borders-p t)
      (setf headers-p t)
      ;; set up the buffer to act the right way
      (with-current-buffer buffer
        (setf cursor-type nil)
        ;; next line changed from (toggle-truncate-lines 1) for
        ;; emacs-21 compatibility.
        (setf truncate-lines t)
        (buffer-disable-undo buffer)
        (make-local-variable 'cell-current-sheet)
        (setf buffer-read-only t)
        (cell-mode-insinuate)
        (cell-sheet-after-open-hook sheet^)
        (setf (buffer-modified-p buffer) nil))
      sheet^)))

;;; The mark and the selection

(defun cell-sheet-set-mark ()
  "Begin defining a region by putting the mark here."
  (interactive)
  (cell-with-current-cell-sheet
   (setf (cell-sheet-mark cell-current-sheet) (list cursor-row cursor-column))
   (cell-sheet-update-selection-from-mark)
   (cell-sheet-update)))

(defun cell-sheet-update-selection-from-mark ()
  "Update the selection based on mark and cursor."
  (cell-with-current-cell-sheet
   (let ((mark (cell-sheet-mark cell-current-sheet)))
     (when mark
       (when (not (and  (= (car mark) cursor-row)
                        (= (cadr mark) cursor-column)))
         (setf (cell-sheet-selection cell-current-sheet)
               (list cursor-row cursor-column
                     (car mark) (cadr mark))))))))

(defun cell-sheet-clear-mark* ()
  "Clear the mark and selection without updating the sheet."
  (cell-with-current-cell-sheet
   (setf (cell-sheet-mark cell-current-sheet) nil)
   (setf (cell-sheet-selection cell-current-sheet) nil)))

(defun cell-sheet-clear-mark ()
  "Stop defining a region by clearing the mark."
  (interactive)
  (cell-sheet-clear-mark*)
  (cell-sheet-update))

;;; Cut, copy, paste

(defun cell-copy-cell (c)
  "Copy the cell C to avoid data sharing."
  (when c
    (let ((class (eieio-object-class c)))
      (with-slots (label value face) c
        (let ((label* (copy-tree label :vec))
              (value* (copy-tree value :vec))
              (face* (copy-tree face :vec)))
          (make-instance class :label label* :value value* :face face*))))))

(defun cell-sheet-copy-to-clipboard ()
  "Copy the currently selected region to the clipboard."
  (interactive)
  (cell-with-current-cell-sheet
   (if (null selection)
       (progn (setf cell-clipboard (list (list (cell-copy-cell (cell-grid-get grid cursor-row cursor-column)))))
              (message "The cell at the cursor was copied to the clipboard."))
     (let* ((inhibit-read-only t))
       (cl-destructuring-bind (r1 c1 r2 c2) selection
         (let* ((dr (abs (- r2 r1)))
                (dc (abs (- c2 c1)))
                (start-row (min r1 r2))
                (start-col (min c1 c2))
                (out-rows ())
                (out-column ()))
           (cl-do ((r start-row (+ 1 r)))
               ((> r (+ dr start-row)))
             (cl-do ((c start-col (+ 1 c)))
                 ((> c (+ dc start-col)))
               (push (cell-copy-cell (cell-grid-get grid r c))
                     out-column))
             (push (nreverse out-column) out-rows)
             (setf out-column nil))
           (setf cell-clipboard (nreverse out-rows))))
       (cell-sheet-clear-mark*)
       (cell-sheet-update)
       (message "The selection was copied to the clipboard.")))))

(defun cell-sheet-blank-selection (&optional sel)
  "Remove all cells within the current selection SEL."
  (interactive)
  (cell-with-current-cell-sheet
   (when (or sel selection)
     (let* ((inhibit-read-only t))
       (cell-push-undo-history)
       (cl-destructuring-bind (r1 c1 r2 c2)
           (or sel (slot-value cell-current-sheet 'selection))
         (let* ((dr (abs (- r2 r1)))
                (dc (abs (- c2 c1)))
                (start-row (min r1 r2))
                (start-col (min c1 c2))
                (out-rows ())
                (out-column ()))
           (cl-do ((r start-row (+ 1 r)))
               ((> r (+ dr start-row)))
             (cl-do ((c start-col (+ 1 c)))
                 ((> c (+ dc start-col)))
               (cell-grid-set grid r c nil)))))))))

(defun cell-sheet-cut-to-clipboard ()
  "Cut the selected cells to the clipboard."
  (interactive)
  (cell-with-current-cell-sheet
   (cell-push-undo-history)
   (if (null (cell-sheet-selection cell-current-sheet))
       (cell-grid-set grid cursor-row cursor-column nil)
     (progn
       (let ((sel (cell-sheet-selection cell-current-sheet)))
         (cell-sheet-copy-to-clipboard)
         (cell-sheet-blank-selection sel))))
   (cell-sheet-clear-mark*)
   (cell-sheet-update)
   (message "The selection was cut to the clipboard.")))

(defun cell-sheet-paste ()
  "Paste the current contents of the clipboard at the cursor."
  (interactive)
  (let ((inhibit-read-only t))
    (cell-push-undo-history)
    (cell-with-current-cell-sheet
     (let ((rows cell-clipboard))
       (cl-do ((r cursor-row (1+ r)))
           ((null rows))
         (let ((col (pop rows)))
           (cl-do ((c cursor-column (1+ c)))
               ((null col))
             (cell-grid-set grid r c (cell-copy-cell (pop col)))))))
     (cell-sheet-clear-mark*)
     (cell-sheet-update))))

;;; Blanking a cell sheet

(cl-defmethod cell-sheet-blank ((sheet cell-sheet) rows cols)
  "Clear the sheet SHEET with a blank one with ROWS and COLS."
  (let ((inhibit-read-only t))
    (setf (cell-sheet-grid sheet)
          (cell-make-grid rows cols))
    (setf (cell-sheet-cursor sheet) (list 0 0))))

;;; The cursor and the selection

(defun cell-move-point-to-rendering ()
  "Move point to the place where the sheet will be rendered."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (get-text-property (point) 'rendering)))
    (forward-char 1))
  (when (not (get-text-property (point) 'rendering))
    (error "Cell-mode: could not find rendering text property"))
  (forward-char 1)
  (point))

(defun cell-sheet-display-cursor (row column &optional face)
  "Display a cursor using overlays at the give ROW and COLUMN.
The argument FACE specifies the face to use for the overlay."
  (cell-with-current-cell-sheet
   (unless (null cell-cached-buffer-positions)
     (let* ((inhibit-read-only t)
          (face (or face 'cell-cursor-face))
          (cursor-width (- (aref stops (+ column 1))
                           (aref stops column))))
       (let ((p (gethash (list row column) cell-cached-buffer-positions)))
         (let* ((cl (cell-grid-get grid row column))
                (p0 0)
                (q (if (and cl (cell-label-width cl))
                       -1
                     0)))
           (let ((ov (make-overlay (- p cursor-width) p)))
             (overlay-put ov 'face face)
             (goto-char p))))))))

(defun cell-sheet-highlight-cell (row column &optional face)
  "Highlight a cell by adding face properties at ROW and COLUMN.
The argument FACE specifies which face to use."
  (cell-with-current-cell-sheet
   (let* ((inhibit-read-only t)
          (cursor-width (if (and cell^ (cell-label-width cell^))
                            (ceiling (cell-label-width cell^))
                          (- (aref stops (+ column 1))
                             (aref stops column))))
          (face (or face 'cell-cursor-face)))
     (cell-move-point-to-rendering)
     ;; move to right place
     (forward-line (+ 1 row))
     (forward-char (aref stops column))
     ;; adjust cursor when images are present in any cells to left
     (let ((n 0))
       (dotimes (c column)
         (let ((cl (cell-grid-get grid row c)))
           (when cl
             (when (cell-label-width cl)
               (cl-incf n)
               (backward-char 1)))))
       ;; (when (plusp n)
       ;;        (goto-char (+ (point-at-bol) (aref stops column)))
       ;;        (forward-char (+ n 1))))
       (add-text-properties (point) (+ (point) cursor-width)
                            (list 'face face))))))

(defun cell-sheet-display-selection ()
  "Display the selection using text properties."
  (cell-with-current-cell-sheet
   (when selection
     (let* ((inhibit-read-only t))
       (cl-destructuring-bind (r1 c1 r2 c2) selection
         (let* ((dr (abs (- r2 r1)))
                (dc (abs (- c2 c1)))
                (start-row (min r1 r2))
                (start-col (min c1 c2))
                (current-cell nil))
           (goto-char (point-min))
           (forward-line (1+ start-row))
           (cl-do ((r start-row (+ r 1)))
               ((> r (+ start-row dr)))
             (beginning-of-line)
             (let* ((pos (+ (point-at-bol) (aref stops start-col)))
                    (end pos))
               (cl-do ((c start-col (+ c 1)))
                   ((> c (+ start-col dc)))
                 (setf current-cell (cell-grid-get grid r c))
                 (let ((cursor-width (if (and current-cell (cell-label-width current-cell))
                                         (ceiling (cell-label-width current-cell))
                                       (- (aref stops (+ c 1))
                                          (aref stops c)))))
                   (cl-incf end cursor-width)))
               (add-text-properties pos end
                                    (list 'face 'cell-selection-face)))
             (forward-line))))))))

(defun cell-sheet-in-selection (rxx cxx)
  "Return non-nil if row RXX, column CXX is within the selection."
  (let ((selection (cell-sheet-selection cell-current-sheet)))
    (when selection
      (let ((r1 (pop selection))
            (c1 (pop selection))
            (r2 (pop selection))
            (c2 (pop selection)))
        (and (<= (min r1 r2) rxx (max r1 r2))
             (<= (min c1 c2) cxx (max c1 c2)))))))

(defun cell-sheet-display-cursors (&optional just-delete)
  "Display all the cursors, or just delete them when JUST-DELETE is non-nil."
  (cell-with-current-cell-sheet
   ;; remove overlays. there is a fix for emacs-21 here, which does
   ;; not have the function (remove-overlays)
   (if (fboundp 'remove-overlays)
       (remove-overlays)
     (mapc #'delete-overlay (overlays-in (point-min) (point-max))))
   (unless just-delete
     (cell-sheet-display-cursor cursor-row cursor-column))))

(defun cell-sheet-move-bol ()
  "Move to the beginning of the line."
  (interactive)
  (cell-with-current-cell-sheet
   (with-slots (cursor) cell-current-sheet
     (setf cursor
           (list (cl-first cursor) 0)))
   (cell-sheet-update)))

(defun cell-sheet-move-eol ()
  "Move to the end of the line."
  (interactive)
  (cell-with-current-cell-sheet
   (with-slots (cursor grid) cell-current-sheet
     (setf cursor
           (list (cl-first cursor)
                 (1- (length (aref grid 0))))))
   (cell-sheet-update)))

(defun cell-sheet-move-bob ()
  "Move to the top left corner of the cell sheet."
  (interactive)
  (cell-with-current-cell-sheet
   (with-slots (cursor) cell-current-sheet
     (setf cursor (list 0 0))
     (cell-sheet-update))))

(defun cell-sheet-move-eob ()
  "Move to the bottom right corner of the cell sheet."
  (interactive)
  (cell-with-current-cell-sheet
   (with-slots (cursor grid) cell-current-sheet
     (setf cursor
           (list (1- (length grid))
                 (1- (length (aref grid 0))))))
   (cell-sheet-update)))

(cl-defun cell-sheet-move-cursor (direction &optional (the-distance 1))
  "Move the cursor one cell in DIRECTION, in THE-DISTANCE units."
  (cell-with-current-cell-sheet
   ;; clear selection
   (setf (cell-sheet-selection cell-current-sheet) nil)
   ;; calculate new cursor location
   (let* ((rows (cell-grid-rows grid))
          (cols (cell-grid-columns grid))
          (new-cursor
           (cl-case direction
             (:up (if (not (cl-minusp (- cursor-row the-distance)))
                      (list (- cursor-row the-distance) cursor-column)
                    cursor))
             (:left (if (not (cl-minusp (- cursor-column the-distance)))
                        (list cursor-row (- cursor-column the-distance))
                      cursor))
             (:down (if (< cursor-row (- rows the-distance))
                        (list (+ cursor-row the-distance) cursor-column)
                      cursor))
             (:right (if (< cursor-column (- cols the-distance))
                         (list cursor-row (+ cursor-column the-distance))
                       cursor)))))
     (setf (cell-sheet-cursor cell-current-sheet) new-cursor)
     ;; choose technique for display
     (if (cell-sheet-raw-display-p cell-current-sheet)
         ;; just move point to where cursor should go
         (progn
           (let ((buffer-position (+ 1 (cl-second new-cursor)
                                     (* (cl-first new-cursor)
                                        (+ 1 (cell-grid-columns grid))))))
             (goto-char buffer-position)))
       ;; display cell-mode cursor with overlay
       (if (cell-sheet-mark cell-current-sheet)
           (progn (cell-remove-rendering*)
                  (cell-sheet-update-selection-from-mark)
                  (cell-sheet-update :lazy)
                  (cell-sheet-display-cursors))
         (progn
           (cell-sheet-display-cursors)))))))
;; (cell-remove-rendering)
;; (cell-sheet-update :lazy)))))))

(defun cell-sheet-move-cursor-up ()
  "Move the cursor one cell upward."
  (interactive)
  (cell-sheet-move-cursor :up))

(defun cell-sheet-move-cursor-left ()
  "Move the cursor one cell leftward."
  (interactive)
  (cell-sheet-move-cursor :left))

(defun cell-sheet-move-cursor-down ()
  "Move the cursor one cell downward."
  (interactive)
  (cell-sheet-move-cursor :down))

(defun cell-sheet-move-cursor-right ()
  "Move the cursor one cell rightward."
  (interactive)
  (cell-sheet-move-cursor :right))

(defun cell-sheet-page-up ()
  "Move the cursor one page upward."
  (interactive)
  ;; check for correct major mode, in case mouse is over a cell-mode
  ;; buffer that isn't the current buffer.
  (when (eq major-mode 'cell-mode)
    (cell-sheet-move-cursor :up 10)))

(defun cell-sheet-page-down ()
  "Move the cursor one page downward."
  (interactive)
  (when (eq major-mode 'cell-mode)
    (cell-sheet-move-cursor :down 10)))

;;; Executing cells

(defun cell-sheet-execute ()
  "Execute the current cell."
  (interactive)
  (cell-with-current-cell-sheet (cell-execute cell^)))

;;; Deleting cells

(defun cell-sheet-delete-cell ()
  "Delete the current cell."
  (interactive)
  (cell-with-current-cell-sheet
   (cell-push-undo-history)
   (cell-grid-set grid cursor-row cursor-column nil)
   (cell-sheet-update)))

;;; Interactively creating and editing cells

(cl-defmethod cell-sheet-after-update-hook ((sheet cell-sheet))
  "This method is invoked after every update to the SHEET."
  nil)

(cl-defmethod cell-sheet-after-open-hook ((sheet cell-sheet))
  "This method is invoked after the SHEET is opened."
  (setf (buffer-modified-p (current-buffer)) nil))

(defun cell-sheet-create-cell ()
  "Create or edit a cell at the cursor."
  (interactive)
  (cell-with-current-cell-sheet
   (cell-push-undo-history)
   (let* ((the-cell (cell-grid-get grid cursor-row cursor-column))
          (value (when the-cell (cell-value the-cell))))
     (let* ((instance (or the-cell (make-instance 'cell-expression)))
            (default (cell-value instance))
            (val (read-from-minibuffer "Enter value: "
                                       (when the-cell default))))
       (when (and (stringp val)
                  (> (length val) 0))
         (cell-grid-set grid cursor-row cursor-column (copy-tree instance :copy-vectors))
         (cell-accept-string-value instance (copy-tree val :copy-vectors)))))
   (cell-sheet-update)))

(defun cell-sheet-create-comment ()
  "Create a comment cell."
  (interactive)
  (cell-with-current-cell-sheet
   (cell-push-undo-history)
   (let ((i (make-instance 'cell-comment-cell))
         (val (read-from-minibuffer "Comment expression: ")))
     (cell-set-value i val)
     (cell-grid-set grid cursor-row cursor-column i)
     (cell-sheet-update))))

(defun cell-sheet-create-image ()
  "Create an image cell at the cursor."
  (interactive)
  (cell-with-current-cell-sheet
   (cell-push-undo-history)
   (let ((i (make-instance 'cell-image-cell))
         (val (read-file-name "Image file name: ")))
     (cell-set-value i val)
     (cell-grid-set grid cursor-row cursor-column i)
     (cell-sheet-update))))

;;; Inserting rows and columns

(defun cell-sheet-insert-row ()
  "Insert a row at the cursor."
  (interactive)
  (cell-push-undo-history)
  (cell-with-current-cell-sheet
   (setf (cell-sheet-grid cell-current-sheet) (cell-grid-insert-row grid cursor-row))
   (cell-sheet-update)))

(defun cell-sheet-insert-column ()
  "Insert a column at the cursor."
  (interactive)
  (cell-push-undo-history)
  (cell-with-current-cell-sheet
   (let ((columns (+ 1 (cell-grid-columns (cell-sheet-grid cell-current-sheet)))))
     (setf (cell-sheet-grid cell-current-sheet) (cell-grid-insert-column grid cursor-column))
     (setf (cell-sheet-column-stops cell-current-sheet) (make-vector (+ 1 columns) 0))))
  (cell-sheet-update))

;;; Deleting rows and columns

(defun cell-sheet-delete-row ()
  "Delete a row at the cursor."
  (interactive)
  (cell-push-undo-history)
  (cell-with-current-cell-sheet
   (setf (cell-sheet-grid cell-current-sheet) (cell-grid-delete-row grid cursor-row))
   (cell-sheet-update)))

(defun cell-sheet-delete-column ()
  "Delete a column at the cursor."
  (interactive)
  (cell-push-undo-history)
  (cell-with-current-cell-sheet
   (setf (cell-sheet-grid cell-current-sheet) (cell-grid-delete-column grid cursor-column))
   (cell-sheet-update)))

;;; Collecting cells

;; This is useful when you want to process the contents of a sheet.

(defun cell-collect-cells (sheet &optional collect-all-p)
  "Return a flat list of all the cells in the SHEET.
If COLLECT-ALL-P is non-nil, ignore the value of each cell's
CELL-COLLECT-VALUE-P."
  (let ((grid (cell-sheet-grid cell-current-sheet))
        (cell^ nil)
        (cells nil))
    (dotimes (r (cell-grid-rows grid))
      (dotimes (c (cell-grid-columns grid))
        (when (setf cell^ (cell-grid-get grid r c))
          (when (and cell^ (or collect-all-p (cell-collect-value-p cell^)))
            (push cell^ cells)))))
    (nreverse cells)))

(defun cell-collect-cells-by-row (sheet &optional collect-all-p)
  "Return a list of rows of all the cells in the SHEET.
If COLLECT-ALL-P is non-nil, ignore the value of each cell's
CELL-COLLECT-VALUE-P."
  (let ((grid (cell-sheet-grid cell-current-sheet))
        (rows nil))
    (dotimes (r (cell-grid-rows grid))
      (let ((cell^ nil)
            (cells nil))
        (dotimes (c (cell-grid-columns grid))
          (when (setf cell^ (cell-grid-get grid r c))
            (when (and cell^ (or collect-all-p (cell-collect-value-p cell^)))
              (push cell^ cells))))
        (when cells (push (nreverse cells) rows))))
    (nreverse rows)))

(defun cell-collect-cells-by-row-include-blanks (sheet)
  "Return a list of rows of all the cells in the SHEET, with blanks."
  (let ((grid (cell-sheet-grid cell-current-sheet))
        (rows nil))
    (dotimes (r (cell-grid-rows grid))
      (let ((cell^ nil)
            (cells nil))
        (dotimes (c (cell-grid-columns grid))
          (setf cell^ (cell-grid-get grid r c))
          (push cell^ cells))
        (when cells (push (nreverse cells) rows))))
    (nreverse rows)))

(defun cell-collect-cells-by-row-no-settings (sheet)
  "Return a list of all the cells in SHEET, without settings cells."
  (let ((rows (cell-collect-cells-by-row sheet))
        (results ()))
    (dolist (row rows)
      (when (not (cl-some #'vectorp (mapcar #'cell-find-value row)))
        (push row results)))
    (nreverse results)))

;;; Drawing

(defun cell-sheet-do-layout ()
  "Compute layout information for the cell sheet."
  (let ((widths (make-vector (cell-grid-columns (cell-sheet-grid cell-current-sheet)) 0))
        (column-width 0))
    (cell-with-current-cell-sheet
     (let ((rows (cell-grid-rows grid))
           (columns (cell-grid-columns grid)))
       ;; Find column widths and stops before rendering.
       ;; how many digits in longest row number?
       (setf (cell-sheet-row-header-width cell-current-sheet)
             (max 2 (length (format "%d" rows))))
       ;; factor in headers if needed
       (setf (aref (cell-sheet-column-stops cell-current-sheet) 0)
             (if (cell-sheet-headers-p cell-current-sheet)
                 (cell-sheet-row-header-width cell-current-sheet)
               0))
       (dotimes (col columns)
         (setf column-width 0)
         (dotimes (row rows)
           (setf cell^ (cell-grid-get grid row col))
           (setf column-width (max column-width (if cell^
                                                    (+ 1 
                                                       (if (cell-label-width cell^)
                                                           (ceiling (cell-label-width cell^))
                                                         (length 
                                                          (if (cell-label cell^)
                                                              (cell-label cell^)
                                                            cell-no-label-string))))
                                                  cell-blank-width))))
         (setf (aref widths col) column-width)
         (when (< col columns)
           (setf (aref (cell-sheet-column-stops cell-current-sheet) (+ 1 col))
                 (+ column-width (aref (cell-sheet-column-stops cell-current-sheet) col)))))
       ;; save layout data
       (setf (cell-sheet-widths cell-current-sheet) widths)))))

(defun cell-sheet-do-redraw ()
  "Redraw the cell sheet."
  (interactive)
  (let* ((inhibit-read-only t)
         (selection-now-p nil)
         (rendering-point nil)
         (grid (cell-sheet-grid cell-current-sheet))
         (rows (cell-grid-rows grid))
         (cursor (cell-sheet-cursor cell-current-sheet))
         (mark (cell-sheet-mark cell-current-sheet))
         (cursor-row (cl-first cursor))
         (cursor-column (cl-second cursor))
         (mark-row (cl-first mark))
         (mark-column (cl-second mark))
         (columns (cell-grid-columns grid))
         (column-stops (cell-sheet-column-stops cell-current-sheet))
         (widths (cell-sheet-widths cell-current-sheet))
         (cell^ nil)
         (headers-p (cell-sheet-headers-p cell-current-sheet))
         (raw-display-p (cell-sheet-raw-display-p cell-current-sheet))
         (column-width 0)
         (cell-width 0)
         (row-header-width (cell-sheet-row-header-width cell-current-sheet))
         (face nil))
    (if raw-display-p 
        ;; just insert all the labels with \n between lines
        (progn 
          (setf cursor-type 'hollow)
          (goto-char (point-min))
          (dotimes (row rows)
            (dotimes (col columns)
              (setf cell^ (cell-grid-get grid row col))
              (when (and cell^ (cell-label cell^))
                (insert (cell-label cell^))))
            (insert "\n"))
          ;; move cursor to right place
          (goto-char (+ 1 cursor-column 
                        (* cursor-row 
                           (+ 1 (cell-grid-columns grid))))))
      ;; otherwise render everything all nice with borders and cell backgrounds. 
      ;;
      ;; Render the cell sheet into the buffer
      (goto-char (point-max))
      (setf rendering-point (point))
      ;; draw headers if needed
      (when headers-p
        (cell-insert-header (cell-sheet-row-header-width cell-current-sheet) 0 :empty)
        (dotimes (col columns)
          (cell-insert-header (- (aref column-stops (+ col 1)) (aref column-stops col)) col)))
      (insert "\n")
      (dotimes (row rows)
        ;; draw headers if needed
        (when headers-p
          (cell-insert-header row-header-width row))
        ;; render
        (dotimes (col columns)
          (setf column-width (aref widths col))
          (setf cell^ (cell-grid-get grid row col))
          (when cell^
            (setf face `(,(or (cell-face cell^) 'cell-default-face)
                         ,(if (cl-evenp col) 'cell-blank-face
                            'cell-blank-odd-face))))
          ;; draw selection face if needed
          (if (cell-sheet-in-selection row col)
              (progn (setf face 'cell-selection-face)
                     (setf selection-now-p t))
            (setf selection-now-p nil))
          ;; ;; or cursor face
          ;; (when (and (= cursor-row row)
          ;;         (= cursor-column col))
          ;;   (setf face 'cell-cursor-face))
          ;; or mark face
          (when (and mark
                     (= mark-row row)
                     (= mark-column col))
            (setf face 'cell-mark-face))
          ;;
          (if cell^
              (progn
                (setf cell-width
                      (cell-insert cell^ face))
                ;; fill column to integer width, if needed
                (if (cell-label-width cell^)
                    (progn 
                      (cell-insert-spacer (+ 1.0 (- (ceiling (cell-label-width cell^))
                                                    (cell-label-width cell^)))
                                          face)
                      ;;(cell-fill-space cell^ face)
                      (cell-insert-blank (- column-width (ceiling (cell-label-width cell^)) 1) face))
                  (cell-insert-blank (- column-width cell-width) face)))
            ;; blank cell
            (progn (setf face (if selection-now-p face (if (cl-evenp col) 'cell-blank-face
                                                         'cell-blank-odd-face)))
                   (setf cell-width (cell-insert-blank column-width face))))
          ;; add text properties to store row, col for mouse click
          (let* ((end (point))
                 (beg (- end column-width)))
            (put-text-property beg end 'cell-mode-position (list row col))))
        (insert "\n"))
      ;;
      (setf (cell-sheet-rendering cell-current-sheet)
            (buffer-substring rendering-point (point-max)))
      (goto-char (point-min))
      (cell-sheet-update-selection-from-mark)
      (cell-sheet-display-cursors))))

(defun cell-sheet-set-raw-display (sheet bool)
  "Enable raw display on SHEET if BOOL is non-nil."
  (setf (cell-sheet-raw-display-p cell-current-sheet) bool))

;;;; Rendering individual elements of the spreadsheet

(defun cell-insert (c &optional face)
  "Make a clickable text cell from C and insert it at point.
Returns length of inserted string.  FACE is the optional face to
use."
  (let* ((label (or (cell-label c) cell-no-label-string))
         (face (or face 'cell-default-face))
         (action `(lambda ()
                    (interactive)
                    nil)))
    (let ((str label)) 
      (insert str)
      ;; the variables ROW and COL are free in the next line.
      (setf (gethash (list row col)
                     cell-cached-buffer-positions)
            (point))
      (add-text-properties (point) (- (point) (length str))
                           `(face ,face))
      (length label))))

(defun cell-insert-blank (width &optional face) 
  "Insert a blank cell WIDTH characters wide in face FACE."
  (when (> width 0)
    (insert (propertize (make-string width ? ) 'face face))
    ;; the variables ROW and COL are free in the next line.
    (setf (gethash (list row col)
                   cell-cached-buffer-positions)
          (point))
    width))

(defun cell-insert-spacer (width &optional face)
  "Insert an arbitrary WIDTH spacer with face FACE."
  (insert "Q")
  (backward-char)
  (put-text-property (point) (1+ (point)) 'display
                     `(space . (:width ,width)))
  (when face
    (put-text-property (point) (1+ (point)) 'face face))
  ;; adjust cached buffer position for cursor, when spacer is inserted
  (cl-incf (gethash (list row col)
                 cell-cached-buffer-positions))
  ;;
  (forward-char))

(defun cell-insert-header (width number &optional empty)
  "Insert a header cell of width WIDTH displaying NUMBER.
If EMPTY is non-nil, don't display the number."
  (let ((blank "")
        (label (if empty " "(format "%d" number)))
        (face (if (cl-evenp number)
                  'cell-axis-face
                'cell-axis-odd-face)))
    (dotimes (i (- width (length label)))
      (setf blank (concat blank " ")))
    (insert (propertize (concat blank label)
                        'face face))))

(defun cell-make-clickable (text action &optional face)
  "Make TEXT clickable calling function ACTION.
The optional argument FACE is the face to use." 
  (let ((keymap (make-sparse-keymap))
        (face (or face 'cell-face-red)))
    (define-key keymap [mouse-2] action)
    (propertize text
                'face face
                'keymap keymap)))

;;; Serialization

(defun cell-remove-rendering* ()
  "Remove the cell sheet's rendering without updating." 
  (let ((inhibit-read-only t))
    (when (eq major-mode 'cell-mode)
      (delete-region (point-min) (point-max)))))

(defun cell-remove-rendering ()
  "Remove the cell sheet's rendering and update."
  (when (eq major-mode 'cell-mode)
    (cell-remove-rendering*)
    (cell-sheet-update-serialization)))

(defun cell-sheet-update* ()
  "Update the cell sheet and mark the buffer non-modified."
  (when (eq major-mode 'cell-mode)
    (let ((inhibit-read-only t))
      (cell-sheet-update)
      (setf (buffer-modified-p (current-buffer)) nil))))

(defun cell-sheet-update-serialization ()
  "Rewrite the buffer contents to reflect the current data."
  (interactive)
  (let ((inhibit-read-only t))
    (cell-with-current-cell-sheet
     (save-excursion
       (delete-region (point-min) (point-max))
       (goto-char (point-min))
       (insert " ")
       (goto-char (point-max))
       (goto-char (point-min))
       (insert (cell-sheet-serialize-eieio cell-current-sheet))
       (goto-char (point-max))
       (insert (propertize " " 'rendering t 'display ""))
       ;;
       (goto-char (point-min))
       (let ((x (progn
                  (while (and (not (eobp))
                              (not (get-text-property (point) 'rendering)))
                    (forward-char 1))
                  (if (not (get-text-property (point) 'rendering))
                      (error "Cell-mode: could not find rendering text property")
                    (point)))))
         (add-text-properties (point-min)
                              x
                              (list 'display "")))
       (setf (buffer-modified-p (current-buffer)) nil)))))

(defun cell-sheet-update (&optional lazy)
  "Update the entire cell sheet.
If LAZY is non-nil, just redraw without updating."
  (interactive)
  (cell-with-current-cell-sheet
   (unless lazy
     (cell-sheet-update-serialization)
     (cell-sheet-do-layout))
   (cell-sheet-do-redraw)
   (cell-sheet-after-update-hook cell-current-sheet)))

(defun cell-sheet-serialize-eieio (sheet)
  "Use EIEIO-PERSISTENT to serialize the SHEET."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (object-write sheet)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun cell-sheet-serialize (sheet)
  "Return a string version of SHEET."
  (let* ((grid (cell-sheet-grid cell-current-sheet))
                 (sheet^ cell-current-sheet)
                 (rows (cell-grid-rows grid))
                 (cols (cell-grid-columns grid))
                 (cell^ nil))
    (cl-labels ((serialize-cell (cell^ row col)
                                (format "(:cell :row %d :col %d :class %S :label %S :value %S)"
                                        row col (eieio-object-class cell^) (when (cell-label cell^)
                                                                             (substring-no-properties (cell-label cell^)))
                                        (cell-value cell^))))
      (with-temp-buffer
        (insert (format "(:sheet :name %S :rows %d :cols %d :raw-display-p %S :grid ("
                        (cell-sheet-name sheet^) rows cols (cell-sheet-raw-display-p sheet^)))
        (cl-dotimes (row rows)
          (cl-dotimes (col cols)
            (when (setf cell^ (cell-grid-get grid row col))
              (insert (serialize-cell cell^ row col))
              (insert "\n"))))
        (insert "\n))")
        (buffer-substring-no-properties (point-min) (point-max))))))

;;; Mouse support

(defun cell-sheet-mouse-move-cursor (event)
  "Move the mouse cursor to the clicked cell.
EVENT is the mouse event data."
  (interactive "e")
  (let ((buf (window-buffer (posn-window (cl-second event)))))
    (let ((old-buffer-modified-p (buffer-modified-p buf)))
      (cell-with-current-cell-sheet
       (when event
         (cl-destructuring-bind (event-type position &optional ignore) event
           (let* ((clicked-position (posn-point position))
                  (clicked-cell (get-text-property clicked-position 'cell-mode-position)))
             ;;
             ;; whether we are in raw display mode
             (when (cell-sheet-raw-display-p cell-current-sheet)
               (goto-char clicked-position)
               ;;
               ;; bounds check 
               (let ((clicked-row (/ clicked-position (+ 1 (cell-grid-columns grid))))
                     (clicked-column (+ -1 (% clicked-position (+ 1 (cell-grid-columns grid))))))
                 (when (and (<= 0 clicked-row)
                            (<= 0 clicked-column)
                            (< clicked-row (cell-grid-rows grid))
                            (< clicked-column (cell-grid-columns grid)))
                   (setf (cell-sheet-cursor cell-current-sheet) 
                         (list clicked-row clicked-column)))))
             ;;
             ;; not in raw display
             (when clicked-cell
               (cl-destructuring-bind (clicked-row clicked-column) clicked-cell
                 (setf (cell-sheet-cursor cell-current-sheet) clicked-cell)
                 (when (cell-sheet-selection cell-current-sheet)
                   (cell-sheet-update-selection-from-mark))
                 (cell-sheet-update)))))))
      (setf (buffer-modified-p buf) old-buffer-modified-p)
      (cell-sheet-after-update-hook cell-current-sheet))))

(defun cell-sheet-mouse-execute (event)
  "Execute the clicked cell.  EVENT is the mouse event data."
  (interactive "e")
  (cell-with-current-cell-sheet
   (when event
     (cl-destructuring-bind (event-type position &optional ignore) event
       (let* ((clicked-position (posn-point position))
              (clicked-cell (get-text-property clicked-position 'cell-mode-position)))
         (when clicked-cell
           (cl-destructuring-bind (clicked-row clicked-column) clicked-cell
             (setf (cell-sheet-cursor cell-current-sheet) clicked-cell)
             (let ((c (cell-grid-get (cell-sheet-grid cell-current-sheet)
                                     clicked-row clicked-column)))
               (when c
                 (cell-tap c)
                 (cell-sheet-update))))))))))

(defun cell-sheet-mouse-select (event)
  "Select a cell with the mouse.  EVENT is the mouse event data."
  (interactive "e")
  (let ((buf (current-buffer)))
    (let ((old-buffer-modified-p (buffer-modified-p (current-buffer))))
      (cell-with-current-cell-sheet
       (cell-sheet-clear-mark*)
       (cl-destructuring-bind (event-type position1 position2 &rest ignore) event
         (let* ((pt1 (get-text-property (posn-point position1) 'cell-mode-position))
                (pt2 (get-text-property (posn-point position2) 'cell-mode-position))
                (sel `(,@pt1 ,@pt2)))
           (setf (cell-sheet-selection cell-current-sheet) sel))
         (cell-sheet-update)))
      (setf (buffer-modified-p buf) old-buffer-modified-p))))

;;; Additional commands

(cl-defun cell-sheet-create (&optional r0 c0 (class 'cell-sheet))
  "Create a new cell sheet with R0 rows and C0 columns, of the chosen CLASS."
  (interactive)
  (let ((r (or r0 (read-from-minibuffer "Create cell-sheet with how many rows? " "24")))
        (c (or c0 (read-from-minibuffer "...with how many columns? " "40"))))
    (let ((rs (car (read-from-string r)))
          (cs (car (read-from-string c))))
      (let ((buffer (generate-new-buffer "*cell sheet*")))
        (switch-to-buffer buffer)
        (cell-mode)
        (setf cell-current-sheet (make-instance class :rows rs :cols cs))
        (cell-sheet-update)))))

(defun cell-mode-with-class (rows cols class)
  "Create a cell-mode sheet with ROWS rows and COLS columns, of the class CLASS."
  (interactive)
  (cell-mode)
  (setf cell-current-sheet (make-instance class :rows rows :cols cols)))

;;; Sheet settings are a form of local variable.

(defvar cell-sheet-class nil "Local mode class for interpreting cell sheets.")
(make-variable-buffer-local 'cell-sheet-class)

(cl-defmethod cell-sheet-collect-settings ((sheet cell-sheet))
  "Return a list of all the settings values in SHEET."
  (let ((cl-values (mapcar #'cell-find-value (cell-collect-cells sheet)))
        (results ()))
    (dolist (value values)
      (when (vectorp value)
        (push value results)))
    (nreverse results)))

(cl-defmethod cell-sheet-collect-settings-cells ((sheet cell-sheet))
  "Return a list of all the settings cells in SHEET."
  (let ((cells (cell-collect-cells sheet :collect-all-p))
        (results ()))
    (dolist (c cells)
      (when (vectorp (cell-find-value c))
        (push c results)))
    results))

(cl-defmethod cell-sheet-change-setting ((sheet cell-sheet) setting-name value)
  "Change the SHEET setting SETTING-NAME to VALUE."
  (let ((cells (cell-sheet-collect-settings-cells sheet)))
    (dolist (c cells)
      (let ((vec (cell-find-value c)))
        (when (eq setting-name (aref vec 0))
          (cell-accept-string-value c (prin1-to-string (vector setting-name value))))))))

(cl-defmethod cell-sheet-setting-value ((sheet cell-sheet) setting-name)
  "Return the value of the SHEET's setting SETTING-NAME."
  (let ((settings (cell-sheet-collect-settings sheet)))
    (cl-block finding
      (dolist (setting settings)
        (when (eq setting-name (aref setting 0))
          (cl-return-from finding (aref setting 1)))))))

(defun cell-apply-settings (settings)
  "Apply each of the variable SETTINGS."
  (dolist (binding settings)
    (set (aref binding 0)
         (aref binding 1))))

(cl-defmethod cell-sheet-apply-settings ((sheet cell-sheet))
  "Apply each of the variable settings in SHEET."
  (with-current-buffer (slot-value sheet 'buffer)
    (cell-apply-settings (cell-sheet-collect-settings sheet))))

(defun cell-sheet-class (sheet)
  "Return the class setting of this SHEET.
As serialization class matching is now handled by
EIEIO-PERSISTENT, this is no longer needed."
  (cell-sheet-setting-value sheet 'cell-sheet-class))

(defun cell-sexp->cell (sexp)
  "Turn the cell expression value SEXP into an expression cell."
  (let (cell)
    (when (and (consp sexp)
               (symbolp (cl-first sexp))
               (ignore-errors (eieio--class-p (eieio--class-object (cl-first sexp)))))
      (setf cell (apply #'make-instance (cl-first sexp) (cl-rest sexp))))
    (when (null cell)
      (setf cell (make-instance 'cell-expression
                                :value sexp
                                :label (if (stringp sexp) sexp (prin1-to-string sexp))))
      (cell-set-value cell sexp))
    cell))

(defun cell-copy-cells-from-sexps (sexps)
  "Copy the cells rows in SEXPS to the clipboard."
  (let ((rows (mapcar (lambda (row)
                        (mapcar #'cell-sexp->cell row))
                      sexps)))
    (setf cell-clipboard rows)))

;;; Customization

(defgroup cell nil
  "Options for cell-mode."
  :group 'applications)

;;; Font locking

(defface cell-default-face '((t (:foreground "black")))
  "Face for cells." :group 'cell)

(defface cell-action-face '((t (:foreground "yellow" :background "red" :bold t :weight bold)))
  "Face for cells that perform an action." :group 'cell)

(defface cell-comment-face '((t (:foreground "dodger blue")))
  "Face for cells that simply label stuff." :group 'cell)

(defface cell-comment-2-face '((t (:foreground "hot pink" :slant italic :italic t)))
  "Face for cells that simply label stuff." :group 'cell)

(defface cell-file-face '((t (:foreground "yellowgreen")))
  "Face for simple links to files." :group 'cell)

(defface cell-cursor-face '((t (:background "hot pink" :foreground "yellow"
                                            :box (:line-width 1 :color "magenta"))))
  "Face for selected cell." :group 'cell)

(defface cell-mark-face '((t (:background "yellow" :foreground "red")))
  "Face for marked cell." :group 'cell)

(defface cell-selection-face '((t (:background "pale green" :foreground "gray20"
                                               :box (:line-width 1 :color "pale green"))))
  "Face for multi-cell selection." :group 'cell)

(defface cell-text-face '((t (:foreground "yellow")))
  "Face for text entry cells." :group 'cell)

(defface cell-blank-face '((t (:background 
                               "wheat" 
                               :box 
                               (:line-width 1 :color "dark khaki"))))
  "Face for blank cells." :group 'cell)

(defface cell-blank-odd-face '((t (:background 
                                   "cornsilk" 
                                   :box 
                                   (:line-width 1 :color "dark khaki"))))
  "Face for blank cells in odd columns" :group 'cell)

(defface cell-axis-face '((t :foreground "gray50" 
                             :background "gray80" 
                             :box (:line-width 1 :color "grey70")))
  "Face for numbered axes."  :group 'cell)

(defface cell-axis-odd-face '((t :foreground "gray50" 
                                 :background "gray90" 
                                 :box (:line-width 1 :color "grey70")))
  "Face for numbered axes in odd columns." :group 'cell)

(easy-menu-define cell-menu global-map
  "Menu for Cell-mode commands."
  '("Sheet"
    ["Create or edit cell" cell-sheet-create-cell]
    ["Create comment" cell-sheet-create-comment]
    ["Undo" cell-sheet-undo]
    ["Redo" cell-sheet-redo]
    ["Set mark" cell-sheet-set-mark]
    ["Clear mark" cell-sheet-clear-mark]
    ["Delete cell at cursor" cell-sheet-delete-cell]
    ["Cut selection" cell-sheet-cut-to-clipboard]
    ["Copy selection" cell-sheet-copy-to-clipboard]
    ["Paste at cursor" cell-sheet-paste]
    ["Insert row" cell-sheet-insert-row]
    ["Insert column" cell-sheet-insert-column]
    ["Delete row" cell-sheet-delete-row]
    ["Delete column" cell-sheet-delete-column]
    ["Move cursor left" cell-sheet-move-cursor-left]
    ["Move cursor right" cell-sheet-move-cursor-right]
    ["Move cursor up" cell-sheet-move-cursor-up]
    ["Move cursor down" cell-sheet-move-cursor-down]
    ["Move to beginning of row" cell-sheet-move-bol]
    ["Move to end of row" cell-sheet-move-eol]
    ["Page up" cell-sheet-page-up]
    ["Page down" cell-sheet-page-down]
    ["Move to top left corner" cell-sheet-move-bob]
    ["Move to bottom right corner" cell-sheet-move-eob]
    ["Re-initialize buffer (fix columns)" cell-mode]))

(defvar cell-mode-tool-bar-map ())

(defun cell-enable-tool-bar-map ()
  "Enable the cell-mode tool bar."
  (setf cell-mode-tool-bar-map
        (when (keymapp tool-bar-map)
          (let ((tool-bar-map (make-sparse-keymap)))
            (tool-bar-add-item
             "left-arrow" 'winner-undo 'winner-undo 
             :label "Back"
             :help "Back to previous window configuration."
             :enable t)
            (tool-bar-add-item
             "right-arrow"
             'winner-redo 'winner-redo 
             :label "Forward"
             :help "Forward to next window configuration."
             :enable t)
            (define-key-after tool-bar-map [cell-separator-1]
              (list 'menu-item "--") 'winner-redo)
            (tool-bar-add-item
             "save" 'save-buffer 'save-buffer 
             :label "Save"
             :help "Save the current sheet."
             :enable t)
            (define-key-after tool-bar-map [cell-separator-2]
              (list 'menu-item "--") 'save-buffer)
            (tool-bar-add-item
             "undo" 'cell-sheet-undo 'cell-sheet-undo
             :label "Undo"
             :help "Undo."
             :enable t)
            (tool-bar-add-item
             "cut" 'cell-sheet-redo 'cell-sheet-redo
             :label "Redo"
             :help "Redo."
             :enable t)
            (define-key-after tool-bar-map [cell-separator-3]
              (list 'menu-item "--") 'cell-sheet-redo)
            (tool-bar-add-item
             "cut" 'cell-sheet-cut-to-clipboard 'cell-sheet-cut-to-clipboard
             :label "Cut"
             :help "Cut the cells to the clipbboard."
             :enable t)
            (tool-bar-add-item
             "copy" 'cell-sheet-copy-to-clipboard 'cell-sheet-copy-to-clipboard
             :label "Copy"
             :help "Copy the cells to the clipbboard."
             :enable t)
            (tool-bar-add-item
             "paste" 'cell-sheet-paste 'cell-sheet-paste
             :label "Paste"
             :help "Paste cells from the clipboard."
             :enable t)
            tool-bar-map))))

(provide 'cell-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; cell-mode.el ends here
