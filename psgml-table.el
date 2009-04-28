;;; psgml-table.el --- Editing table for SGML-mode
;;
;; $Id:

;; Copyright (C) 1999 Erik Helander

;; Author: Erik Helander <Erik.Helander@telia.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Commentary:

;; Part of major mode for editing the SGML document-markup language.

;;;; Code:

(require 'psgml-parse)
(provide 'psgml-table)

(eval-when-compile
  (setq byte-compile-warnings '(free-vars unresolved
                                          callargs redefine)))

(defconst psgml-enter-table-version "1"
  "Version of psgml package.")

(defvar sgml-start-table nil)
(defvar sgml-main-buffer nil)
(defvar sgml-table-values nil)
(defvar sgml-table-parse-state t)

(defun sgml-enter-table ()
  "Enter tables in current document.
Entering table data is done in a separate window."
  (interactive)
  (let ((start (point-marker)))
    ;; Check if table is allowed
    (cond (sgml-table-parse-state
           (sgml-parse-to-here)
           (cond ((member "TABLE"
                          (mapcar '(lambda (x) (format "%s" x))
                                  (sgml-optional-tokens
                                   sgml-current-state)))
                  (sgml-enter-table-go start))
                 (t
                  (message "TABLE is not valid here"))))
          (t
           (sgml-enter-table-go start)))
    ))

(defun sgml-enter-table-go (start)
  "Main function for sgml-enter-table"
  (switch-to-buffer-other-window
   (sgml-enter-table-data-buffer))
  (sgml-enter-table-data-mode)
  (make-local-variable 'sgml-start-table)
  (setq sgml-start-table start))

(defun sgml-enter-table-data-buffer ()
  (let ((bname "*Enter table data*")
        (buf nil)
        (inhibit-read-only t))
    (save-excursion
      (when (setq buf (get-buffer bname))
        (kill-buffer buf))
      (setq buf (get-buffer-create bname))
      (set-buffer buf)
      (erase-buffer)
      (sgml-insert '(read-only t rear-nonsticky (read-only))
"Table data input area\n -- Enter values and finish with C-c C-c --")
      (sgml-insert '(read-only t rear-nonsticky (read-only))
                   "\n Columns     = ")
      (sgml-insert nil "1")
      (sgml-insert '(read-only 1) "\n")
      (sgml-insert '(read-only t rear-nonsticky (read-only))
                   " Header Rows = ")
      (sgml-insert nil "0")
      (sgml-insert '(read-only 1) "\n")
      (sgml-insert '(read-only t rear-nonsticky (read-only))
                   " Body Rows   = ")
      (sgml-insert nil "1")
      (sgml-insert '(read-only 1) "\n")
      (sgml-insert '(read-only t rear-nonsticky (read-only))
                   " Footer Rows = ")
      (sgml-insert nil "0")
      (sgml-insert '(read-only 1) "\n")
      (sgml-insert '(read-only t rear-nonsticky (read-only))
                   " Type = ")
      (sgml-insert nil "CALS")
      (sgml-insert '(read-only 1) "\n")
      (sgml-insert '(read-only t)
                   " Type can be CALS or HTML depending on your DTD.")
      (sgml-insert '(read-only t)
                   "\n -- End of table data input area --")
      (goto-char (point-min))
      (sgml-enter-table-data-next))
    buf))

(defvar sgml-enter-table-data-mode-map (make-sparse-keymap))
(define-key sgml-enter-table-data-mode-map "\C-c\C-c"
  'sgml-enter-table-data-finish)
(define-key sgml-enter-table-data-mode-map "\C-c\C-k"
  'sgml-enter-table-data-clear)

(define-key sgml-enter-table-data-mode-map "\C-a"
  'sgml-enter-table-data-field-start)
(define-key sgml-enter-table-data-mode-map "\C-e"
  'sgml-enter-table-data-field-end)
(define-key sgml-enter-table-data-mode-map "\t"
  'sgml-enter-table-data-next)

(defun sgml-enter-table-data-mode ()
  "Major mode to enter table data.\\<sgml-enter-table-data-mode-map>
Use \\[sgml-enter-table-data-next] to move between input fields.
To abort, kill buffer (\\[kill-buffer]) and remove window
\(\\[delete-window]).  To finsh use \\[sgml-enter-table-data-finish].

\\{sgml-enter-table-data-mode-map}"
  (setq mode-name "SGML enter table data mode"
        major-mode 'sgml-enter-table-data-mode)
  (use-local-map sgml-enter-table-data-mode-map)
  (run-hooks 'text-mode-hook 'sgml-enter-table-data-mode-hook))

(defun sgml-enter-table-data-finish ()
  "Finish entering values and puts the table in original buffer."
  (interactive)
  (let ((cb (current-buffer))
        (vals (sgml-get-entered-table-data))
        ;; save buffer local variables
        (start sgml-start-table))
    (when (markerp start)
      (delete-windows-on cb)
      (switch-to-buffer (marker-buffer start))
      (kill-buffer cb)
      (goto-char start)
      ;; Enter the values and tags...
      (sgml-insert-table-with-values vals)
      )))

(defun sgml-enter-table-data-clear ()
  "Kill the value of current data."
  (interactive)
  (kill-region
   (progn (sgml-enter-table-data-field-start) (point))
   (progn (sgml-enter-table-data-field-end) (point))))

(defun sgml-enter-table-data-field-start ()
  "Go to the start of the current data field."
  (interactive)
  (let (start)
        (beginning-of-line 1)
    (while (not (eq t (get-text-property (point) 'read-only)))
      (beginning-of-line 0))
    (setq start (next-single-property-change (point) 'read-only))
    (unless start (error "No value here"))
    (assert (number-or-marker-p start))
    (goto-char start)))

(defun sgml-enter-table-data-field-end ()
  "Go to the end of the current data field."
  (interactive)
  (sgml-enter-table-data-field-start)
  (let ((end (if (and (eolp)
                      (get-text-property (1+ (point)) 'read-only))
                 (point)
               (next-single-property-change (point) 'read-only))))
    (assert (number-or-marker-p end))
    (goto-char end)))

(defun sgml-enter-table-data-next ()
  "Move to next data field."
  (interactive)
  (unless (search-forward-regexp
"^ *[_.:A-Za-z0-9---]+ *[_.:A-Za-z0-9---]* *= ?" nil t nil)
      (goto-char (point-min))
      (sgml-enter-table-data-next)))

(defun sgml-get-entered-table-data ()
  (let (val
        vals)
    (goto-char (point-min))
    (sgml-enter-table-data-next)
    (save-excursion
      (save-restriction
        (narrow-to-region (point)
                          (progn (sgml-enter-table-data-field-end)
                                 (point)))
        (setq val (buffer-string))))
    (push val vals)
    (sgml-enter-table-data-next)
    (save-excursion
      (save-restriction
        (narrow-to-region (point)
                          (progn (sgml-enter-table-data-field-end)
                                 (point)))
        (setq val (buffer-string))))
    (push val vals)
    (sgml-enter-table-data-next)
    (save-excursion
      (save-restriction
        (narrow-to-region (point)
                          (progn (sgml-enter-table-data-field-end)
                                 (point)))
        (setq val (buffer-string))))
    (push val vals)
    (sgml-enter-table-data-next)
    (save-excursion
      (save-restriction
        (narrow-to-region (point)
                          (progn (sgml-enter-table-data-field-end)
                                 (point)))
        (setq val (buffer-string))))
    (push val vals)
    (sgml-enter-table-data-next)
    (save-excursion
      (save-restriction
        (narrow-to-region (point)
                          (progn (sgml-enter-table-data-field-end)
                                 (point)))
        (setq val (buffer-string))))
    (push val vals)
    vals))

(defun sgml-insert-table-with-values (vals)
  (let (val
        cols
        total-cols
        rows
        head
        body
        foot
        repeat
        el)
    (setq val (pop vals))
    (cond ((equal val "CALS")
           (setq total-cols (string-to-int (cadddr vals)))
           (sgml-insert nil
                        (sgml-general-insert-case
                         (format "<TABLE><TGROUP COLS=%d>"
                                 total-cols)))
           ;; COLSPEC
           (setq cols 0)
           (setq repeat (make-list total-cols 'a))
           (loop
            for i in repeat do
            (setq cols (1+ cols))
            (sgml-insert nil
                        (sgml-general-insert-case
                         (format
"\n<COLSPEC COLNAME=\"C%d\" COLWIDTH=\"\">"
                                 cols))))
           ;; THEAD
           (when (> (setq head (string-to-int (caddr vals))) 0)
             (sgml-insert nil
                          (sgml-general-insert-case
                           "\n<THEAD>"))
             (setq rows 0)
             (setq repeat (make-list head 'a))
             (loop
              for i in repeat do
              (setq rows (1+ rows))
              (sgml-insert nil (sgml-general-insert-case
                                "\n<ROW>"))
              (setq cols 0)
              (setq repeat (make-list total-cols 'a))
              (loop
               for i in repeat do
               (setq cols (1+ cols))
               (sgml-insert nil
                            (sgml-general-insert-case
                             (format "<ENTRY COLNAME=\"C%d\">"
                                     cols)))
               (sgml-insert nil (sgml-general-insert-case
                                 "</ENTRY>")))
              (sgml-insert nil (sgml-general-insert-case
                                "\n</ROW>")))
             (sgml-insert nil (sgml-general-insert-case
                               "\n</THEAD>")))
           ;; TFOOT
           (when (> (setq foot (string-to-int (car vals))) 0)
             (sgml-insert nil (sgml-general-insert-case
                               "\n<TFOOT>"))
             (setq rows 0)
             (setq repeat (make-list foot 'a))
             (loop
              for i in repeat do
              (setq rows (1+ rows))
              (sgml-insert nil (sgml-general-insert-case
                                "\n<ROW>"))
              (setq cols 0)
              (setq repeat (make-list total-cols 'a))
              (loop
               for i in repeat do
               (setq cols (1+ cols))
               (sgml-insert nil
                            (sgml-general-insert-case
                             (format "<ENTRY COLNAME=%d>"
                                     cols)))
               (sgml-insert nil (sgml-general-insert-case
                                 "</ENTRY>")))
              (sgml-insert nil (sgml-general-insert-case
                                "\n</ROW>")))
             (sgml-insert nil (sgml-general-insert-case
                               "\n</TFOOT>")))
           ;; TBODY
           (sgml-insert nil (sgml-general-insert-case
                             "\n<TBODY>"))
           (setq rows 0)
           (setq repeat (make-list (string-to-int
                                    (cadr vals)) 'a))
           (loop
            for i in repeat do
            (setq rows (1+ rows))
            (sgml-insert nil (sgml-general-insert-case
                              "\n<ROW>"))
            (setq cols 0)
            (setq repeat (make-list total-cols 'a))
            (loop
             for i in repeat do
             (setq cols (1+ cols))
             (sgml-insert nil
                            (sgml-general-insert-case
                             (format "<ENTRY COLNAME=%d>"
                                     cols)))
             (sgml-insert nil (sgml-general-insert-case
                               "</ENTRY>")))
            (sgml-insert nil (sgml-general-insert-case
                              "\n</ROW>")))
           (sgml-insert nil (sgml-general-insert-case
                             "\n</TBODY>"))
           (sgml-insert nil (sgml-general-insert-case
                             "\n</TGROUP></TABLE>")))
          ((equal val "HTML")
           (setq total-cols (string-to-int (cadddr vals)))
           (sgml-insert nil (sgml-general-insert-case
                             "<TABLE>"))
           ;; TH
           (when (> (setq head (string-to-int (caddr vals))) 0)
             (setq rows 0)
             (setq repeat (make-list head 'a))
             (loop
              for i in repeat do
              (setq rows (1+ rows))
              (sgml-insert nil (sgml-general-insert-case
                                "\n<TR>"))
              (setq cols 0)
              (setq repeat (make-list total-cols 'a))
              (loop
               for i in repeat do
               (setq cols (1+ cols))
               (sgml-insert nil (sgml-general-insert-case
                                 "<TH></TH>")))
              (sgml-insert nil (sgml-general-insert-case
                                "\n</TR>"))))
           ;; Body
           (when (> (setq body (string-to-int (cadr vals))) 0)
             (setq rows 0)
             (setq repeat (make-list body 'a))
             (loop
              for i in repeat do
              (setq rows (1+ rows))
              (sgml-insert nil (sgml-general-insert-case
                                "\n<TR>"))
              (setq cols 0)
              (setq repeat (make-list total-cols 'a))
              (loop
               for i in repeat do
               (setq cols (1+ cols))
               (sgml-insert nil (sgml-general-insert-case
                                 "<TD></TD>")))
              (sgml-insert nil (sgml-general-insert-case
                                "\n</TR>"))))
           ;; TFOOT IS NOT DEFINED IN HTML?
           (when (> (string-to-int (car vals)) 0)
             (sgml-insert nil
                          "<!-- No footer in HTML tables -->"))
           (sgml-insert nil (sgml-general-insert-case
                             "\n</TABLE>")))
          (t
           ))))

(defun sgml-enter-table-parse-on ()
  (interactive)
  (setq sgml-table-parse-state t))

(defun sgml-enter-table-parse-off ()
  (interactive)
  (setq sgml-table-parse-state nil))

(defvar sgml-general-insert-case 'lower) ;for old packages...
(defvar sgml-namecase-general t)         ;such as 1.0.1
(defun sgml-general-insert-case (text)   ;redefinition ...
  (if sgml-namecase-general              ;same reason
      (case sgml-general-insert-case
        (upper (upcase text))
        (lower (downcase text))
        (t text))
    text))
