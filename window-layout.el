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
;; (setq strct 
;;       (wlf:layout 
;;        '(| folder (- summary message))
;;        (list (make-wlf:window 
;;               :name 'folder 
;;               :buffer "folder buffer")
;;              (make-wlf:window 
;;               :name 'summary
;;               :buffer "summary buffer"
;;               :size 10)
;;              (make-wlf:window
;;               :name 'message
;;               :buffer "message buffer"
;;               :show nil))))
;;
;; (wlf:show   strct 'summary)
;; (wlf:hide   strct 'summary)
;; (wlf:toggle strct 'summary)
;; (wlf:select strct 'summary)
;;
;; (wlf:get-buffer strct 'summary)
;; (wlf:set-buffer strct 'summary "*scratch*")

;; structure: 
;;   (split recipi) (window info)

;;; Code:

(defmacro wlf:aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'wlf:aif 'lisp-indent-function 2)

;; window management structure
;; name : the symbol of window name.
;; buffer : buffer object to show the window
;; max-size : if window size is larger than this value, the window is shrinked.
;; default-show : if nil, the window should be not displayed initially.
;; *shown : [internal use] 'show/'hide. if 'hide, the window is not displayed.
;; *window : [internal use] window object.
;; *vertical : [internal use] if the window is splitted vertically, the value is t.

(defstruct wlf:window 
  name buffer max-size (default-show t) 
  *shown *window *vertical)

(defun wlf:window-*shown-set (winfo i)
  "[internal] translate the argument: nil -> 'hide / t -> 'show"
  (setf (wlf:window-*shown winfo) (if i 'show 'hide)))

(defun wlf:window-*shown-p (winfo)
  "[internal] Return t, if the window should be shown."
  (eq 'show (wlf:window-*shown winfo)))

(defun wlf:window-*shown-toggle (winfo)
  "[internal] Toggle window displaying state."
  (setf (wlf:window-*shown winfo)
        (if (wlf:window-*shown-p winfo) 'hide 'show)))

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
          (unless (wlf:window-*shown winfo)
            (wlf:window-*shown-set winfo (wlf:window-default-show winfo)))
          (setf (wlf:window-*window winfo) former-window)
          (setf (wlf:window-*vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec former-recipi winfo-list))
    (select-window latter-window)
    (if (symbolp latter-recipi)
        (let ((winfo (wlf:get-winfo latter-recipi winfo-list)))
          (unless (wlf:window-*shown winfo)
            (wlf:window-*shown-set winfo (wlf:window-default-show winfo)))
          (setf (wlf:window-*window winfo) latter-window)
          (setf (wlf:window-*vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec latter-recipi winfo-list))
    ))

(defun wlf:apply-winfo (winfo)
  "[internal] Apply window layout."
  (let ((window (selected-window)))
    (if (not (wlf:window-*shown-p winfo))
        (delete-window window)
      (switch-to-buffer (get-buffer (wlf:window-buffer winfo)))
      ;; apply maximum size
      (wlf:aif
          (wlf:window-max-size winfo)
          (cond
           ((wlf:window-*vertical winfo)
            (let ((size (window-height window)))
              (if (< (wlf:window-max-size winfo) size)
                  (shrink-window (- size (wlf:window-max-size winfo))))))
           (t
            (let ((size (window-width window)))
              (if (< (wlf:window-max-size winfo) size)
                  (shrink-window-horizontally (- size (wlf:window-max-size winfo)))))))))))

(defun wlf:layout (recipi window-info)
  "Build layout management object."
  (let ((last-buffer (current-buffer)) val)
    (save-excursion
      (wlf:clear-windows)
      (wlf:build-windows-rec recipi window-info)
      (setq val (cons recipi window-info)))
    (wlf:aif (get-buffer-window last-buffer)
        (select-window it))
    val))

(defun wlf:show (structure winfo-name)
  "Display the window."
  (let* ((recipi (car structure))
         (winfo-list (cdr structure))
         (winfo (wlf:get-winfo winfo-name winfo-list)))
    (wlf:window-*shown-set winfo t)
    (wlf:layout recipi winfo-list)))

(defun wlf:hide (structure winfo-name) 
  "Hide the window."
  (let* ((recipi (car structure))
         (winfo-list (cdr structure))
         (winfo (wlf:get-winfo winfo-name winfo-list)))
    (wlf:window-*shown-set winfo nil)
    (wlf:layout recipi winfo-list)))

(defun wlf:toggle (structure winfo-name)
  "Toggle the window."
  (let* ((recipi (car structure))
         (winfo-list (cdr structure))
         (winfo (wlf:get-winfo winfo-name winfo-list)))
    (wlf:window-*shown-toggle winfo)
    (wlf:layout recipi winfo-list)))

(defun wlf:select (structure winfo-name)
  "Select the window."
  (select-window
   (wlf:window-*window
    (wlf:get-winfo winfo-name (cdr structure)))))

(defun wlf:set-buffer (structure winfo-name buf)
  "Set the buffer on the window."
  (let ((winfo (wlf:get-winfo winfo-name (cdr structure))))
    (setf (wlf:window-buffer winfo) buf)
    (select-window (wlf:window-*window winfo))
    (switch-to-buffer buf)))

(defun wlf:get-buffer (structure winfo-name)
  "Return the buffer object on the window.
This function uses the structure data, not currently displayed
window."
  (wlf:window-buffer (wlf:get-winfo winfo-name (cdr structure))))

;;; for test

;; (setq s 
;;       (wlf:layout
;;        '(| folder (- summary message))
;;        (list (make-wlf:window 
;;               :name 'folder 
;;               :buffer "*info*"
;;               :max-size 20)
;;              (make-wlf:window 
;;               :name 'summary
;;               :buffer "*Messages*"
;;               :max-size 10)
;;              (make-wlf:window
;;               :name 'message
;;               :buffer "window-layout.el"
;;               :default-show t))))

;; (wlf:show s 'message)
;; (wlf:hide s 'message)
;; (wlf:toggle s 'message)
;; (wlf:select s 'summary)
;; (wlf:get-buffer s 'message)
;; (wlf:set-buffer s 'message "*scratch*")


(provide 'window-layout)
;;; window-layout.el ends here
