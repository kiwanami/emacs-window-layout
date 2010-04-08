;;; window-layout.el --- window layout manager

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai@kiwanami.net>
;; Version: 1.0
;; Keywords: window, layout

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

;; Split a frame or window into some windows according to a layout
;; recipe.

;;; Example code

;; ;; Layout function
;; ; -> three pane layout.
;; (setq wm ; <-- window management object
;;       (wlf:layout 
;;        '(| folder (- summary message))
;;        '((:name 'folder 
;;           :buffer "folder buffer")
;;          (:name 'summary
;;           :buffer "summary buffer"
;;           :max-size 15)
;;          (:name 'message
;;           :buffer "message buffer"
;;           :default-hide nil))))
;;
;; ;; Window controlling
;; (wlf:show    wm 'summary)
;; (wlf:hide    wm 'summary)
;; (wlf:toggle  wm 'summary)
;; (wlf:select  wm 'summary)
;; (wlf:refresh wm)
;;
;; ;; Accessing the buffer
;; (wlf:get-buffer wm 'summary) -> <#buffer object>
;; (wlf:set-buffer wm 'summary "*scratch*")

;; ;; Layout hook
;; (defun wlf:test-hook (wset) (message "HOOK : %s" wset))
;; (wlf:layout-hook-add wm 'wlf:test-hook)
;; (wlf:layout-hook-remove wm 'wlf:test-hook)

;;; `wlf:layout' function

;; * Layout recipe:

;;   - : split vertically
;;   | : split horizontally

;; * Window options:

;;   :name  [*] the window name
;;   :buffer  a buffer name or a buffer object to show the window
;;   :size  (column or row number) window size
;;   :max-size  (column or row number) if window size is larger than this value, the window is shrunken.
;;   :size-ratio  (0.0 - 1.0) window size ratio. the size of the other side is the rest.
;;   :default-hide  (t/nil) if t, the window is hided initially. (default: nil)
;;   :fix-size  (t/nil) if t, when the windows are laid out again, the window size is remained. (default: nil)
;; 
;; Note: 
;; The size parameters, :size, :max-size and :size-ratio, are mutually
;; exclusive.  The size of a window is related with one of the other
;; side window. So, if both side windows set size parameters, the
;; window size may not be adjusted as you write.

;; * wholep option:

;; If this option is nil or omitted, this function splits the windows
;; within the current window. If this option is non-nil, this function
;; uses the entire space of the current frame. Because some user
;; actions and complicated window layouts may cause unexpected
;; split behaviors, it is easy to use the entire space of a frame.

;; * Return value (Window management object):

;; You should not access the management object directly, because it is not 
;; intended direct access.
;; You can make some management objects to switch the window layout.

;; * Layout hook

;; When splitting windows, registered hook are called with one
;; argument, the window management object. 

;;; Code:

(eval-when-compile (require 'cl))

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

;;; Window-set management structure
;; recipe      : an input recipe object.
;; winfo-list  : a list of window management structures.
;; wholep      : if non nil, this function uses whole frame window.
;; layout-hook : if doing layout windows, these hooks are called. 
;;               The hook function has one argument: wset object.

(defstruct wlf:wset recipe winfo-list wholep layout-hook)

;;; Window management structure
;; name      : a symbol of the window name.
;; options   : an option alist given by the recipe.
;; shown     : 'show/'hide. if 'hide, the window is not displayed.
;; window    : a window object.
;; vertical  : if the window is split vertically, the value is t.
;; last-size : if the window is alive, the window size is saved before laying out.

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
    (with-selected-window window
      (cond
       ((wlf:window-vertical winfo)
        (let ((current-size (window-height window)))
          (shrink-window
           (- current-size target-size))))
       (t
        (let ((current-size (window-width window)))
          (shrink-window-horizontally 
           (- current-size target-size))))))))

(defmacro wlf:window-option-get (winfo option-key)
  "[internal] Return an option value from an option property list."
  `(plist-get (wlf:window-options ,winfo) ',option-key))

(defun wlf:clear-windows (winfo-list wholep)
  "[internal] Destroy windows and return the window object to
start dividing."
  (cond 
   (wholep ; using the whole area 
    (delete-other-windows)
    (selected-window))
   (t      ; nested windows
    (let ((wins 
           (loop for i in winfo-list
                 for win = (wlf:window-window i)
                 if (and win (window-live-p win))
                 collect win)))
      (if (> (length wins) 1)
          (loop for w in (cdr wins)
                unless (one-window-p)
                do (delete-window w)))
      (or (car wins) (selected-window))))))

(defun wlf:get-winfo (name winfo-list)
  "[internal] Select a window info object from a winfo list."
  (wlf:aif 
      (loop for i in winfo-list
            when (eq (wlf:window-name i) name)
            return i) it
    (error "Window name %s is not found." name)))

(defun wlf:build-windows-rec (recipe winfo-list)
  "[internal] Split the selected window with the recipe."
  (let* 
      ((split-type (car recipe))
       (split-action 
        (cond
         ((eq '- split-type) 'split-window-vertically)
         ((eq '| split-type) 'split-window-horizontally)
         (t 'split-window-vertically)))
       (former-recipe (cadr recipe))
       (latter-recipe (caddr recipe))
       (latter-window (funcall split-action))
       (former-window (selected-window)))

    (unless (window-live-p former-window)
      (error "Can not create a window (former-window is not live)"))
    (select-window former-window)
    (if (symbolp former-recipe)
        (let ((winfo (wlf:get-winfo former-recipe winfo-list)))
          (unless (wlf:window-shown winfo)
            (wlf:window-shown-set winfo (null (wlf:window-option-get winfo :default-hide))))
          (setf (wlf:window-window winfo) former-window)
          (setf (wlf:window-vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec former-recipe winfo-list))

    (unless (window-live-p latter-window)
      (error "Can not create a window (latter-window is not live.)"))
    (select-window latter-window)
    (if (symbolp latter-recipe)
        (let ((winfo (wlf:get-winfo latter-recipe winfo-list)))
          (unless (wlf:window-shown winfo)
            (wlf:window-shown-set winfo (null (wlf:window-option-get winfo :default-hide))))
          (setf (wlf:window-window winfo) latter-window)
          (setf (wlf:window-vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec latter-recipe winfo-list))
    ))

(defun wlf:apply-winfo (winfo)
  "[internal] Apply window layout."
  (if (not (wlf:window-shown-p winfo))
      (delete-window (selected-window))
    (wlf:aif (wlf:window-option-get winfo :buffer)
        (switch-to-buffer (get-buffer it)))
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
      (wlf:window-resize winfo (wlf:window-last-size winfo)))))

(defun wlf:make-winfo-list (wparams)
  "[internal] Return a list of window info objects."
  (loop for p in wparams
        collect (make-wlf:window 
                 :name (plist-get p ':name)
                 :options p)))

(defun wlf:save-current-window-sizes (recipe winfo-list)
  "[internal] Save current window sizes, before clearing the windows."
  (loop for winfo in winfo-list
        do (setf (wlf:window-last-size winfo) nil))
  (wlf:aif
   (frame-parameter (selected-frame) 'wlf:recipe)
   (if (equal recipe it)
       (loop for winfo in winfo-list
             do (setf (wlf:window-last-size winfo)
                      (if (and (wlf:window-window winfo) 
                               (window-live-p (wlf:window-window winfo)))
                          (wlf:window-size winfo) nil)))))
  (set-frame-parameter (selected-frame) 'wlf:recipe recipe))

(defun wlf:layout (recipe window-params &optional wholep)
  "Lay out windows and return a management object.
RECIPE is a structure of splitting windows. 
WINDOW-PARAMS is a list of the window layout parameters.
If WHOLEP is non-nil, this function uses the entire space of the current frame.
If WHOLEP is nil, this function splits the windows within the current window.
See the comment text to know the further information about parameters.
"
  (wlf:layout-internal (wlf:no-layout recipe window-params wholep)))

(defun wlf:no-layout (recipe window-params &optional wholep)
  "Just return a management object, does not change window
layout. See the comment of `wlf:layout' function for arguments."
  (let ((winfo-list (wlf:make-winfo-list window-params)))
    (make-wlf:wset :recipe recipe 
                   :winfo-list winfo-list
                   :wholep wholep)))

(defun wlf:layout-internal (wset)
  "[internal] Lay out windows and return a management object."
  (wlf:with-wset wset
    (let ((last-buffer (current-buffer)) val)
      (wlf:save-current-window-sizes recipe winfo-list)
      (select-window (wlf:clear-windows winfo-list wholep))
      (wlf:build-windows-rec recipe winfo-list)
      (setq val (make-wlf:wset :recipe recipe 
                               :winfo-list winfo-list
                               :wholep wholep))
      
      (loop for h in (wlf:wset-layout-hook wset)
            do (funcall h wset))
      
      (wlf:aif (get-buffer-window last-buffer)
          (select-window it))
      val)))

(defmacro wlf:with-wset (wset &rest body)
  (declare (indent 1))
  `(let* 
       ((recipe (wlf:wset-recipe wset))
        (winfo-list (wlf:wset-winfo-list wset))
        (wholep (wlf:wset-wholep wset))
        (layout-hook (wlf:wset-layout-hook wset)))
     ,@body))

(defun wlf:layout-hook-add (wset func)
  "Add FUNC to layout-hook of the WSET, and return the layout-hook. 
The function FUNC should have one argument : wset object."
  (let ((hook (wlf:wset-layout-hook wset)))
    (unless (member func hook) 
      (setf (wlf:wset-layout-hook wset) (cons func hook)))
    (wlf:wset-layout-hook wset)))

(defun wlf:layout-hook-remove (wset func)
  "Remove FUNC from layout-hook of the WSET, and return the layout-hook."
  (let ((hook (wlf:wset-layout-hook wset)))
    (when (member func hook)
      (setf (wlf:wset-layout-hook wset) (remove func hook)))
    (wlf:wset-layout-hook wset)))

(defun wlf:refresh (wset)
  "Refresh the window layout. WSET is a management object which
is returned by `wlf:layout'."
  (wlf:layout-internal wset))

(defun wlf:show (wset winfo-name)
  "Display the window. WSET is the management object which is
returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (wlf:window-shown-set 
   (wlf:get-winfo
    winfo-name (wlf:wset-winfo-list wset)) t)
  (wlf:layout-internal wset))

(defun wlf:hide (wset winfo-name) 
  "Hide the window. WSET is the management object which
is returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (wlf:window-shown-set
   (wlf:get-winfo 
    winfo-name (wlf:wset-winfo-list wset)) nil)
  (wlf:layout-internal wset))

(defun wlf:toggle (wset winfo-name)
  "Toggle the window. WSET is the management object which
is returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (wlf:window-shown-toggle
   (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset)))
  (wlf:layout-internal wset))

(defun wlf:select (wset winfo-name)
  "Select the window. WSET is the management object which
is returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (select-window
   (wlf:window-window
    (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset)))))

(defun wlf:set-buffer (wset winfo-name buf)
  "Set the buffer on the window. WSET is the management object
which is returned by `wlf:layout'. WINFO-NAME is the window name
which is defined by the argument of `wlf:layout'. BUF is a buffer
name or object to show in the window."
  (let ((winfo 
         (wlf:get-winfo 
          winfo-name (wlf:wset-winfo-list wset))))
    (plist-put (wlf:window-options winfo) :buffer buf)
    (select-window (wlf:window-window winfo))
    (switch-to-buffer buf)))

(defun wlf:get-buffer (wset winfo-name)
  "Return the buffer object on the window.
This function uses the structure data, not currently displayed
window. WSET is the management object which is returned by
`wlf:layout'. WINFO-NAME is the window name which is defined by
the argument of `wlf:layout'."
  (wlf:window-option-get 
   (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset)) :buffer))

(defun wlf:wopts-replace-buffer (wopts buffer-alist)
  "Helper function for the argument of `wlf:layout'. This
function replaces or adds buffer objects in the window options.
WOPTS is a window option list. BUFFER-ALIST is an alist of pairs
of a window name and a buffer object (or buffer name)."
  (loop 
   for pair in buffer-alist
   for name = (car pair)
   for buf = (cdr pair)
   for opts = (loop
               for i in wopts 
               if (eq (plist-get i ':name) name) 
               return i)
   do (plist-put opts ':buffer buf))
  wopts)

;;; test

;; (setq ss
;;       (wlf:layout
;;        '(| folder (- summary message))
;;        '((:name folder :buffer "*info*" :max-size 20)
;;          (:name summary :buffer "*Messages*" :max-size 10)
;;          (:name message :buffer "window-layout.el" :default-hide nil)) t))

;; (setq dd
;;       (wlf:no-layout
;;        '(| folder (| summary message))
;;        '((:name folder :buffer "*info*" :size-ratio 0.33)
;;          (:name summary :buffer "*Messages*" :size-ratio 0.5)
;;          (:name message :buffer "window-layout.el")) t))

;; (wlf:show ss 'folder)
;; (wlf:hide ss 'folder)
;; (wlf:toggle ss 'folder)
;; (wlf:select ss 'summary)
;; (wlf:get-buffer ss 'message)
;; (wlf:set-buffer ss 'message "*scratch*")
;; (wlf:refresh ss)
;; (wlf:refresh dd)

;; (wlf:wopts-replace-buffer 
;;  '((:name folder :buffer "*info*" :max-size 20)
;;    (:name summary :buffer "*Messages*" :max-size 10)
;;    (:name message :buffer "window-layout.el" :default-hide nil))
;;  '((folder . "*Messages*") (summary . "*scratch*")))

;; (defun wlf:test-hook (wset) (message "HOOK : %s" wset))
;; (wlf:layout-hook-add ss 'wlf:test-hook)
;; (wlf:layout-hook-remove ss 'wlf:test-hook)

(provide 'window-layout)
;;; window-layout.el ends here
