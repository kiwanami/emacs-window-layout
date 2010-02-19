;;; window-layout.el --- window layout manager

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <sakurai@liza>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example code
;; 
;; ;; Layout function
;; (setq strct 
;;       (wlf:layout 
;;        '(| folder (- summary message))
;;        '((:name 'folder 
;;           :buffer "folder buffer")
;;          (:name 'summary
;;           :buffer "summary buffer"
;;           :max-size 10)
;;          (:name 'message
;;           :buffer "message buffer"
;;           :default-show nil))))
;;
;; ;; Window control
;; (wlf:show   strct 'summary)
;; (wlf:hide   strct 'summary)
;; (wlf:toggle strct 'summary)
;; (wlf:select strct 'summary)
;;
;; ;; Accessing the buffer
;; (wlf:get-buffer strct 'summary)
;; (wlf:set-buffer strct 'summary "*scratch*")
;;
;; Layout recipi:
;;   - : split veritically
;;   | : split holizontally
;;
;; Window options:
;;   :name  [*] the window name
;;   :buffer  [*] a buffer name or a buffer object to show the window
;;   :size  (column or row number) window size
;;   :max-size  (column or row number) if window size is larger than this value, the window is shrinked.
;;   :size-ratio  (0 - 1.0) window size ratio. the size of the other side is the rest.
;;   :default-hide  (t/nil) if nil, the window should be not displayed initially.
;;   :fix-size  (t/nil) if t, when the windows are re-layouted, the window size is not changed.
;;
;; Note: 
;; The size parameters, :size, :max-size and :size-ratio, are mutually
;; exclusive.  The size of a window is related with one of the other
;; side window. So, if both side windows set size parameters, the
;; window size may not be adjusted as you write.

;;; Code:

(defmacro wlf:aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'wlf:aif 'lisp-indent-function 2)

(defmacro wlf:acond (&rest clauses)
  (if (null clauses) nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym)) ,@(cdr cl1))
           (wlf:acond ,@(cdr clauses)))))))
(put 'wlf:acond 'lisp-indent-function 2)

;;; Window management structure
;; name      : the symbol of window name.
;; options   : option alist
;; shown     : 'show/'hide. if 'hide, the window is not displayed.
;; window    : window object.
;; vertical  : if the window is splitted vertically, the value is t.
;; last-size : if the window is alive, the window size is saved before re-layouting.

(defstruct wlf:window name options shown window vertical last-size)

(defun wlf:window-shown-set (winfo i)
  "[internal] translate the argument: nil -> 'hide / t -> 'show"
  (setf (wlf:window-shown winfo) (if i 'show 'hide)))

(defun wlf:window-shown-p (winfo)
  "[internal] Return t, if the window should be shown."
  (eq 'show (wlf:window-shown winfo)))

(defun wlf:window-shown-toggle (winfo)
  "[internal] Toggle window displaying state."
  (setf (wlf:window-shown winfo)
        (if (wlf:window-shown-p winfo) 'hide 'show)))

(defun wlf:window-size (winfo)
  "[internal] Return current window size."
  (let ((window (wlf:window-window winfo)))
    (cond
     ((wlf:window-vertical winfo)
      (window-height window))
     (t
      (window-width window)))))

(defun wlf:window-shrink (winfo shrink-size)
  "[internal] Shrink window size."
  (let ((window (wlf:window-window winfo)))
    (cond
     ((wlf:window-vertical winfo)
      (shrink-window shrink-size))
     (t
      (shrink-window-horizontally shrink-size)))))

(defun wlf:window-resize (winfo target-size)
  "[internal] Resize window."
  (let ((window (wlf:window-window winfo)))
    (cond
     ((wlf:window-vertical winfo)
      (let ((current-size (window-height window)))
        (shrink-window
         (- current-size target-size))))
     (t
      (let ((current-size (window-width window)))
        (shrink-window-horizontally 
         (- current-size target-size)))))))

(defmacro wlf:window-option-get (winfo option-key)
  "[internal] Return an option value."
  `(plist-get (wlf:window-options ,winfo) ',option-key))

(defun wlf:clear-windows ()
  "[internal] Destory windows and return last one window object."
  (let ((frame (selected-frame)))
    (while (not (one-window-p))
      (delete-window))
    (selected-window)))

(defun wlf:get-winfo (name winfo-list)
  "[internal] Select a window info object from a winfo list."
  (wlf:aif 
   (loop for i in winfo-list
         when (eq (wlf:window-name i) name)
         return i) it
         (error "Window name %s is not found." name)))

(defun wlf:build-windows-rec (recipi winfo-list)
  "[internal] Split the selected window with the recipi."
  (let* (
         (split-type (car recipi))
         (split-action 
          (cond
           ((eq '- split-type) 'split-window-vertically)
           ((eq '| split-type) 'split-window-horizontally)
           (t 'split-window-vertically)))
         (former-recipi (cadr recipi))
         (latter-recipi (caddr recipi))
         (latter-window (funcall split-action))
         (former-window (selected-window))
         )
    (select-window former-window)
    (if (symbolp former-recipi)
        (let ((winfo (wlf:get-winfo former-recipi winfo-list)))
          (unless (wlf:window-shown winfo)
            (wlf:window-shown-set winfo (null (wlf:window-option-get winfo :default-hide))))
          (setf (wlf:window-window winfo) former-window)
          (setf (wlf:window-vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec former-recipi winfo-list))
    (select-window latter-window)
    (if (symbolp latter-recipi)
        (let ((winfo (wlf:get-winfo latter-recipi winfo-list)))
          (unless (wlf:window-shown winfo)
            (wlf:window-shown-set winfo (null (wlf:window-option-get winfo :default-hide))))
          (setf (wlf:window-window winfo) latter-window)
          (setf (wlf:window-vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec latter-recipi winfo-list))
    ))

(defun wlf:apply-winfo (winfo)
  "[internal] Apply window layout."
  (let ((window (selected-window)))
    (if (not (wlf:window-shown-p winfo))
        (delete-window window)
      (switch-to-buffer (get-buffer (wlf:window-option-get winfo :buffer)))
      ;; apply size
      (if (or (wlf:window-option-get winfo :fix-size)
              (null (wlf:window-last-size winfo)))
          ;; set size
          (wlf:acond
           ((wlf:window-option-get winfo :max-size)
            (let ((size (wlf:window-size winfo)))
              (if (< it size)
                  (wlf:window-shrink winfo (- size it)))))
           ((wlf:window-option-get winfo :size)
            (wlf:window-resize winfo it))
           ((wlf:window-option-get winfo :size-ratio)
            (wlf:window-resize winfo (truncate (* 2 (wlf:window-size winfo) it)))))
        ;; revert size
        (wlf:window-resize winfo (wlf:window-size winfo))))))

(defun wlf:make-winfo-list (wparams)
  "[internal] Return a list of window info objects."
  (loop for p in wparams
        collect (make-wlf:window 
                 :name (plist-get p ':name)
                 :options p)))

(defun wlf:save-current-window-sizes (structure)
  "[internal] Save current window sizes."
  (loop for winfo in (cdr structure)
        do (setf (wlf:window-last-size winfo)
                 (if (window-live-p (wlf:window-window winfo))
                     (wlf:window-size winfo)))))

(defun wlf:layout (recipi window-params)
  "Layout windows and make management object."
  (let ((winfo-list (wlf:make-winfo-list window-params)))
    (wlf:layout-internal recipi winfo-list)))

(defun wlf:layout-internal (recipi winfo-list)
  "[internal] Layout windows and make management object."
  (let ((last-buffer (current-buffer)) val)
    (save-excursion
      (wlf:clear-windows)
      (wlf:build-windows-rec 
       recipi winfo-list)
      (setq val (cons recipi winfo-list)))
                                        ; Structure: 
                                        ;   ((split recipi) . (window info))
    (wlf:aif (get-buffer-window last-buffer)
        (select-window it))
    val))

(defun wlf:show (structure winfo-name)
  "Display the window."
  (let* ((recipi (car structure))
         (winfo-list (cdr structure))
         (winfo (wlf:get-winfo winfo-name winfo-list)))
    (wlf:window-shown-set winfo t)
    (wlf:layout-internal recipi winfo-list)))

(defun wlf:hide (structure winfo-name) 
  "Hide the window."
  (let* ((recipi (car structure))
         (winfo-list (cdr structure))
         (winfo (wlf:get-winfo winfo-name winfo-list)))
    (wlf:window-shown-set winfo nil)
    (wlf:layout-internal recipi winfo-list)))

(defun wlf:toggle (structure winfo-name)
  "Toggle the window."
  (let* ((recipi (car structure))
         (winfo-list (cdr structure))
         (winfo (wlf:get-winfo winfo-name winfo-list)))
    (wlf:window-shown-toggle winfo)
    (wlf:layout-internal recipi winfo-list)))

(defun wlf:select (structure winfo-name)
  "Select the window."
  (select-window
   (wlf:window-window
    (wlf:get-winfo winfo-name (cdr structure)))))

(defun wlf:set-buffer (structure winfo-name buf)
  "Set the buffer on the window."
  (let ((winfo (wlf:get-winfo winfo-name (cdr structure))))
    (setf (wlf:window-option-get winfo :buffer) buf)
    (select-window (wlf:window-window winfo))
    (switch-to-buffer buf)))

(defun wlf:get-buffer (structure winfo-name)
  "Return the buffer object on the window.
This function uses the structure data, not currently displayed
window."
  (wlf:window-option-get (wlf:get-winfo winfo-name (cdr structure)) :buffer))

;;; for test

;; (setq s 
;;       (wlf:layout
;;        '(| folder (- summary message))
;;        '((:name folder :buffer "*info*" :max-size 20)
;;          (:name summary :buffer "*Messages*" :max-size 10)
;;          (:name message :buffer "window-layout.el" :default-hide nil))))

;; (setq ss 
;;       (wlf:layout
;;        '(| folder (| summary message))
;;        '((:name folder :buffer "*info*" :size-ratio 0.33)
;;          (:name summary :buffer "*Messages*" :size-ratio 0.5)
;;          (:name message :buffer "window-layout.el"))))

;; (wlf:show ss 'message)
;; (wlf:hide s 'message)
;; (wlf:toggle s 'message)
;; (wlf:select s 'summary)
;; (wlf:get-buffer s 'message)
;; (wlf:set-buffer s 'message "*scratch*")


(provide 'window-layout)
;;; window-layout.el ends here
