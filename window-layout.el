;;; window-layout.el --- window layout manager

;; Copyright (C) 2010, 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Version: 1.1
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
;;        '(| (:left-size-ratio 0.3) 
;;            folder 
;;            (- (:upper-max-size 15) 
;;               summary 
;;               message))
;;        '((:name 'folder 
;;           :buffer "folder buffer")
;;          (:name 'summary
;;           :buffer "summary buffer")
;;          (:name 'message
;;           :buffer "message buffer")
;;         )))

;; ;; Window controlling
;; (wlf:show    wm 'summary)
;; (wlf:hide    wm 'summary)
;; (wlf:toggle  wm 'summary)
;; (wlf:select  wm 'summary)
;; (wlf:toggle-maximize  wm 'summary)

;; ;; Window updating
;; (wlf:refresh wm)
;; (wlf:reset-window-sizes wm)
;; (wlf:reset-init wm)

;; ;; Accessing a buffer
;; (wlf:get-buffer wm 'summary) -> <#buffer object>
;; (wlf:set-buffer wm 'summary "*scratch*")

;; ;; Accessing a window
;; (wlf:get-window wm 'summary)

;; ;; Layout hook
;; (defun wlf:test-hook (wset) (message "HOOK : %s" wset))
;; (wlf:layout-hook-add wm 'wlf:test-hook)
;; (wlf:layout-hook-remove wm 'wlf:test-hook)

;;; `wlf:layout' function

;; * Layout recipe:

;; ( (split type) (split option) 
;;                (left window name or recipe)
;;                (right window name or recipe) )

;;   - : split vertically
;;   | : split horizontally

;; split option (the prefix 'left' can be replaced by 'right', 'upper' and 'lower'.)
;;   :left-size  (column or row number) window size
;;   :left-max-size  (column or row number) if window size is larger than this value, the window is shrunken.
;;   :left-size-ratio  (0.0 - 1.0) window size ratio. the size of the other side is the rest.
;; 
;; Note: 
;; The split option can be omitted.
;; The size parameters, :size, :max-size and :size-ratio, are mutually
;; exclusive.  The size of a window is related with one of the other
;; side window. So, if both side windows set size parameters, the
;; window size may not be adjusted as you write.

;; * Window options:

;;   :name  [*] the window name.
;;   :buffer  a buffer name or a buffer object to show the window. If nil or omitted, the current buffer remains.
;;   :default-hide  (t/nil) if t, the window is hided initially. (default: nil)
;;   :fix-size  (t/nil) if t, when the windows are laid out again, the window size is remained. (default: nil)

;; * subwindow-p option:

;; If this option is not nil, this function splits the windows within
;; the current window. If this option is nil or omitted, this function
;; uses the entire space of the current frame. Because some user
;; actions and complicated window layouts may cause unexpected split
;; behaviors, it is easy to use the entire space of a frame.

;; * Return value (Window management object):

;; You should not access the management object directly, because it is not 
;; intended direct access.
;; You can make some management objects to switch the window layout.

;; * Layout hook

;; After splitting windows, registered hook are called with one
;; argument, the window management object. 



;;; Code:

(eval-when-compile (require 'cl))

(defmacro wlf:aif (test-form then-form &rest else-forms)
  (declare (debug ("test-form" form "then-form" form &rest form))
           (indent 2))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro wlf:acond (&rest clauses)
  (declare (debug (&rest form)))
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
;; options   : an option plist given by the recipe.
;; shown     : 'show/'hide. if 'hide, the window is not displayed.
;; window    : a window object.
;; vertical  : if the window is split vertically, the value is t.
;; last-size : if the window is alive, the window size is saved before laying out.
;; edges     : a list of window edges returned by `window-edges'.

(defstruct wlf:window name options shown window vertical last-size edges)

(defun wlf:window-shown-set (winfo i)
  "[internal] translate the argument: nil -> 'hide / t -> 'show"
  (setf (wlf:window-shown winfo) (if i 'show 'hide)))

(defun wlf:window-shown-p (winfo)
  "[internal] Return t, if the window should be shown."
  (eq 'show (wlf:window-shown winfo)))

(defun wlf:window-shown-toggle (winfo)
  "[internal] Toggle window displaying state."
  (setf (wlf:window-shown winfo)
        (if (and (wlf:window-shown-p winfo) 
                 (wlf:window-live-window winfo))
                 'hide 'show)))

(defun wlf:window-window-by-edge (winfo)
  "[internal] Return a window object which corresponding to WINFO.
This function retrieves the window object from the edge position
in current frame."
  (and (wlf:window-edges winfo)
       (destructuring-bind
           (left top right bottom) (wlf:window-edges winfo)
         (let ((swin (window-at (+ 2 left) (+ 2 top))))
           (and swin 
                (destructuring-bind
                    (sl st sr sb) (window-edges swin)
                  (if (and (equal left sl) (equal top st)) t
                    (message "OLD:%S  NEW:%S"
                             (wlf:window-edges winfo) (window-edges swin)) nil))
                swin)))))

(defun wlf:window-live-window (winfo)
  "[internal] Return a window object if the window is not null and
alive, return nil otherwise."
  (let ((win (wlf:window-window winfo)))
    (and (wlf:window-shown-p winfo) win (window-live-p win) win)))

(defun wlf:window-size (winfo)
  "[internal] Return current window size."
  (let ((window (wlf:window-window winfo)))
    (cond
     ((wlf:window-vertical winfo)
      (window-height window))
     (t
      (window-width window)))))

(defmacro wlf:window-option-get (winfo option-key)
  "[internal] Return an option value from an option property list."
  `(plist-get (wlf:window-options ,winfo) ',option-key))



(defun wlf:clear-windows (winfo-list wholep)
  "[internal] Destroy windows and return the window object to
start dividing."
  (cond 
   (wholep ; using the whole area 
    (delete-other-windows (get-largest-window))
    (get-largest-window))
   (t      ; nested windows
    (let ((wins 
           (loop for i in winfo-list
                 for win = (wlf:window-live-window i)
                 if win
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
       (verticalp (eq 'split-window-vertically split-action))
       (split-options (cadr recipe))
       (recipe-nodes  (cddr recipe))
       (former-recipe (car recipe-nodes))
       (latter-recipe (cadr recipe-nodes))
       (latter-window (funcall split-action))
       (former-window (selected-window)))

    (unless (window-live-p former-window)
      (error "Can not create a window (former-window is not live)"))
    (select-window former-window)
    (when (and split-options
               (plist-get split-options ':leftp))
      (wlf:apply-split-options split-options verticalp))
    (if (symbolp former-recipe)
        (let ((winfo (wlf:get-winfo former-recipe winfo-list)))
          (unless (wlf:window-shown winfo)
            (wlf:window-shown-set winfo (null (wlf:window-option-get winfo :default-hide))))
          (setf (wlf:window-window winfo) former-window)
          (setf (wlf:window-vertical winfo) verticalp)
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec former-recipe winfo-list))

    (unless (window-live-p latter-window)
      (error "Can not create a window (latter-window is not live.)"))
    (select-window latter-window)
    (when (and split-options
               (plist-get split-options ':rightp))
      (wlf:apply-split-options split-options verticalp))
    (if (symbolp latter-recipe)
        (let ((winfo (wlf:get-winfo latter-recipe winfo-list)))
          (unless (wlf:window-shown winfo)
            (wlf:window-shown-set winfo (null (wlf:window-option-get winfo :default-hide))))
          (setf (wlf:window-window winfo) latter-window)
          (setf (wlf:window-vertical winfo) (eq 'split-window-vertically split-action))
          (wlf:apply-winfo winfo))
      (wlf:build-windows-rec latter-recipe winfo-list))
    ))

(defun wlf:apply-split-options (split-options verticalp)
  "[internal] Apply split options to the current window."
  (let ((size (if verticalp
                  (window-height)
                (window-width))))
    (condition-case err
        (wlf:acond
         ((plist-get split-options ':max-size)
          (if (< it size)
              (wlf:window-shrink (selected-window) 
                                 verticalp (- size it))))
         ((plist-get split-options ':size)
          (wlf:window-resize (selected-window) verticalp it))
         ((plist-get split-options ':size-ratio)
          (wlf:window-resize 
           (selected-window) verticalp
           (truncate (* 2 size it)))))
      (error (message "wlf:warning : %s" err)))))

(defun wlf:window-shrink (window verticalp shrink-size)
  "[internal] Shrink window size."
  (cond
   (verticalp
    (shrink-window shrink-size))
   (t
    (shrink-window-horizontally shrink-size))))

(defun wlf:window-resize (window verticalp target-size)
  "[internal] Resize window."
  (with-selected-window window
    (cond
     (verticalp
      (let ((current-size (window-height window)))
        (shrink-window
         (- current-size target-size))))
     (t
      (let ((current-size (window-width window)))
        (shrink-window-horizontally 
         (- current-size target-size)))))))

(defun wlf:apply-winfo (winfo)
  "[internal] Apply layout options to the current window."
  (if (not (wlf:window-shown-p winfo))
      (delete-window (selected-window))
    (wlf:aif (wlf:window-option-get winfo :buffer)
        (when (buffer-live-p (get-buffer it))
          (set-window-buffer (selected-window) (get-buffer it))))))

(defun wlf:collect-window-edges (winfo-list)
  "[internal] At the end of window laying out, this function is
called to collect window edges."
  (loop for winfo in winfo-list
        if (wlf:window-live-window winfo)
        do 
        (setf (wlf:window-edges winfo)
              (window-edges (wlf:window-window winfo)))))

(defun wlf:calculate-last-window-sizes (winfo-list)
  "[internal] Calculate summations of the last window size: width and height.
Return a cons cell, car is width and cdr is height."
  (loop for winfo in winfo-list
        with width = 0 with height = 0
        for size = (wlf:window-last-size winfo)
        if size
        do (cond
            ((wlf:window-vertical winfo)
             (incf height size))
            (t
             (incf width size)))
        finally return (cons width height)))

(defun wlf:calculate-init-window-sizes (winfo-list)
  "[internal] Calculate summations of the initial window size: width and height.
Return a cons cell, car is width and cdr is height."
  (loop for winfo in winfo-list
        with width = 0 with height = 0
        for win = (wlf:window-live-window winfo)
        if win
        do (cond
            ((wlf:window-vertical winfo)
             (incf height (window-height win)))
            (t
             (incf width (window-width win))))
        finally return (cons width height)))

(defun wlf:restore-window-sizes (winfo-list)
  "[internal] Restore the window sizes those are modified by the user."
  ;;checking window layout modification
  (let* ((init-size (wlf:calculate-init-window-sizes winfo-list))
         (last-size (wlf:calculate-last-window-sizes winfo-list))
         (total-width (car init-size))
         (total-height (cdr init-size))
         (width-remainp  (eql (car last-size) total-width))
         (height-remainp (eql (cdr last-size) total-height)))
    ;;restore window size
    (loop for winfo in winfo-list
          for win = (wlf:window-live-window winfo)
          for to-size = (wlf:window-last-size winfo)
          for verticalp = (wlf:window-vertical winfo)
          do
          (when (and win to-size
                     (null (wlf:window-option-get winfo :fix-size))
                     (if verticalp
                         (and height-remainp (not (eql to-size total-height)))
                       (and width-remainp (not (eql to-size total-width)))))
            (wlf:window-resize win verticalp to-size)))))

(defun wlf:make-winfo-list (wparams)
  "[internal] Return a list of window info objects."
  (loop for p in wparams
        collect (make-wlf:window 
                 :name (plist-get p ':name)
                 :options p)))

(defun wlf:translate-recipe (recipe)
  "[internal] Translate split options recursively. 
:left-foo, :upper-foo  -->  :leftp t :foo 
:right-foo, :lower-foo  -->  :rightp t :foo 
"
  (if (or (symbolp recipe) (null recipe))
      recipe
    (let* 
        (split-options 
         new-split-options
         (recipe-nodes
          (if (= 3 (length recipe))
              (cdr recipe)
            (setq split-options (cadr recipe))
            (cddr recipe))))
      (when split-options
        (loop for i in split-options
              do
              (if (symbolp i)
                  (let* ((label-name (symbol-name i)))
                    (wlf:acond
                     ((string-match ":\\(left\\|upper\\)-" label-name)
                      (setq label-name
                            (concat ":" (substring label-name (match-end 0))))
                      (push ':leftp new-split-options)
                      (push t new-split-options))
                     ((string-match ":\\(right\\|lower\\)-" label-name)
                      (setq label-name
                            (concat ":" (substring label-name (match-end 0))))
                      (push ':rightp new-split-options)
                      (push t new-split-options)))
                    (push (intern label-name) new-split-options))
                (push i new-split-options))))
      (list (car recipe) (nreverse new-split-options)
            (wlf:translate-recipe (car recipe-nodes))
            (wlf:translate-recipe (cadr recipe-nodes))))))

(defun wlf:max-window-size-p (winfo)
  "[internal] If current window size is equal to the frame
size (maximum window size), return t. Otherwise return nil."
  (let ((wsize (wlf:window-size winfo)))
    (cond
     ((wlf:window-vertical winfo)
      (>= wsize (1- (frame-height))))
     (t
      (>= wsize (1- (frame-width)))))))

(defun wlf:save-current-window-sizes (recipe winfo-list)
  "[internal] Save current window sizes, before clearing the
windows. The saved sizes are used at `wlf:restore-window-sizes'."
  (loop for winfo in winfo-list
        do (setf (wlf:window-last-size winfo) nil))
  (wlf:aif
      (frame-parameter (selected-frame) 'wlf:recipe)
      (if (equal recipe it)
          (loop for winfo in winfo-list do
                (setf (wlf:window-last-size winfo)
                      (and (wlf:window-live-window winfo)
                           (wlf:max-window-size-p winfo)
                           (wlf:window-size winfo))))))
  (set-frame-parameter (selected-frame) 'wlf:recipe recipe))



(defun wlf:layout (recipe window-params &optional subwindow-p)
  "Lay out windows and return a management object.
RECIPE is a structure of splitting windows. 
WINDOW-PARAMS is a list of the window layout parameters.
If SUBWINDOW-P is nil, this function uses the entire space of the current frame.
If SUBWINDOW-P is non-nil, this function splits the windows within the current window.
See the comment text to know the further information about parameters.
"
  (wlf:layout-internal (wlf:no-layout recipe window-params subwindow-p)))

(defun wlf:no-layout (recipe window-params &optional subwindow-p)
  "Just return a management object, does not change window
layout. See the comment of `wlf:layout' function for arguments."
  (make-wlf:wset :recipe (wlf:translate-recipe recipe)
                 :winfo-list (wlf:make-winfo-list window-params)
                 :wholep (not subwindow-p)))

(defmacro wlf:with-wset (wset &rest body)
  (declare (debug ("wset" sexp &rest form))
           (indent 1))
  `(let* 
       ((recipe (wlf:wset-recipe wset))
        (winfo-list (wlf:wset-winfo-list wset))
        (wholep (wlf:wset-wholep wset))
        (layout-hook (wlf:wset-layout-hook wset)))
     ,@body))

(defun wlf:layout-internal (wset &optional restore-window-size)
  "[internal] Lay out windows and return a management object.
If RESTORE-WINDOW-SIZE is not nil, this function does not restore
the current window size which can be modified by users."
  (wlf:maximize-info-clear)
  (wlf:with-wset wset
    (let ((last-buffer (current-buffer)) val)
      (wlf:save-current-window-sizes recipe winfo-list)
      (select-window (wlf:clear-windows winfo-list wholep))
      (wlf:build-windows-rec recipe winfo-list)
      (unless restore-window-size
        (wlf:restore-window-sizes winfo-list))
      (wlf:collect-window-edges winfo-list)
      (setq val (make-wlf:wset :recipe recipe 
                               :winfo-list winfo-list
                               :wholep wholep))
      
      (loop for h in (wlf:wset-layout-hook wset)
            do (funcall h wset))
      
      (wlf:aif (get-buffer-window last-buffer)
          (select-window it))
      val)))

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

(defun wlf:reset-window-sizes (wset)
  "Reset the window sizes by window recipe parameters."
  (wlf:layout-internal wset t))

(defun wlf:reset-init (wset)
  "Reset the window sizes and display statuses by window recipe parameters."
  (loop for winfo in (wlf:wset-winfo-list wset)
        do (setf (wlf:window-shown winfo) nil))
  (wlf:layout-internal wset t))

(defun wlf:show (wset &rest winfo-names)
  "Display the window. WSET is the management object which is
returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (loop for wn in winfo-names
        do
        (wlf:window-shown-set 
         (wlf:get-winfo
          wn (wlf:wset-winfo-list wset)) t))
  (wlf:layout-internal wset))

(defun wlf:hide (wset &rest winfo-names) 
  "Hide the window. WSET is the management object which
is returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (loop for wn in winfo-names
        do
        (wlf:window-shown-set
         (wlf:get-winfo 
          wn (wlf:wset-winfo-list wset)) nil))
  (wlf:layout-internal wset))

(defun wlf:toggle (wset &rest winfo-names)
  "Toggle the window. WSET is the management object which
is returned by `wlf:layout'. WINFO-NAME is the window name which is
defined by the argument of `wlf:layout'."
  (loop for wn in winfo-names
        do
        (wlf:window-shown-toggle
         (wlf:get-winfo wn (wlf:wset-winfo-list wset))))
  (wlf:layout-internal wset))

(defun wlf:select (wset winfo-name)
  "Select the indicated window. WSET is the management object
which is returned by `wlf:layout'. WINFO-NAME is the window name
which is defined by the argument of `wlf:layout'. If the window
is nil or deleted, no window is selected."
  (wlf:aif 
      (wlf:window-live-window
       (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset)))
      (select-window it)))

(defun wlf:get-window (wset winfo-name)
  "Return the indicated window. WSET is the management object
which is returned by `wlf:layout'. WINFO-NAME is the window name
which is defined by the argument of `wlf:layout'. If the window
is nil or deleted, return nil. Note that after the other window
configuration is applied by `set-window-configuration', this
function may return nil because the window configured by wlf is
not alive. Since many functions calls `set-window-configuration',
programs depend on `wlf:get-window' should watch invocations of
`set-window-configuration'."
  (wlf:window-live-window 
   (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset))))

(defun wlf:set-buffer (wset winfo-name buf &optional selectp)
  "Set the buffer on the window. WSET is the management object
which is returned by `wlf:layout'. WINFO-NAME is the window name
which is defined by the argument of `wlf:layout'. BUF is a buffer
name or object to show in the window."
  (let* ((winfo 
          (wlf:get-winfo 
           winfo-name (wlf:wset-winfo-list wset)))
         (window (wlf:window-live-window winfo)))
    (unless buf (error "Buffer is null! at wlf:set-buffer. (%s)" winfo-name))
    (plist-put (wlf:window-options winfo) :buffer buf)
    (when (and window (not (eql (get-buffer buf) (window-buffer window))))
      (set-window-buffer window buf))
    (when selectp 
      (select-window window))
    window))

(defun wlf:get-buffer (wset winfo-name)
  "Return the buffer object on the window.
This function uses the structure data, not currently displayed
window. WSET is the management object which is returned by
`wlf:layout'. WINFO-NAME is the window name which is defined by
the argument of `wlf:layout'."
  (wlf:window-option-get 
   (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset)) :buffer))

(defun wlf:window-name-p (wset winfo-name)
  "Return t if WINFO-NAME exists in WSET. Otherwise return nil."
  (if (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset)) t
    nil))

(defun wlf:window-displayed-p (wset winfo-name)
  "Return t if the window of WINFO-NAME is displayed. Otherwise return nil."
  (wlf:aif (wlf:get-winfo winfo-name (wlf:wset-winfo-list wset))
      (wlf:window-shown-p it)
    nil))

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

(defun wlf:copy-windows (wset)
  "Return a copied wset object for `set-window-configuration' hacking."
  (make-wlf:wset 
   :recipe (wlf:wset-recipe wset)
   :winfo-list (loop for i in (wlf:wset-winfo-list wset)
                     collect (wlf:copy-winfo i))
   :wholep (wlf:wset-wholep wset)
   :layout-hook (wlf:wset-layout-hook wset)))

(defun wlf:copy-winfo (winfo)
  "[internal] Return a shallow copied window object."
  (make-wlf:window
   :name (wlf:window-name winfo)
   :options (wlf:window-options winfo)
   :shown (wlf:window-shown winfo)
   :window (wlf:window-window winfo)
   :vertical (wlf:window-vertical winfo)
   :last-size (wlf:window-last-size winfo)
   :edges (wlf:window-edges winfo)))

(defun wlf:maximize-info-get ()
  "[internal] Return toggle-maximize info."
  (frame-parameter (selected-frame) 'wlf:maximize))

(defun wlf:maximize-info-set (val)
  "[internal] Set toggle-maximize info."
  (set-frame-parameter (selected-frame) 'wlf:maximize val))

(defun wlf:maximize-info-clear ()
  "[internal] Clear toggle-maximize info."
  (wlf:maximize-info-set nil))

(defun wlf:collect-window-states (wset)
  "[internal] Collect current window states."
  (loop for winfo in (wlf:wset-winfo-list wset)
        collect (cons (wlf:window-name winfo)
                      (wlf:window-shown-p winfo))))

(defun wlf:revert-window-states (wset states)
  "[internal] Revert window states whose are collected by
`wlf:collect-window-states'."
  (loop for s in states
        with winfo-list = (wlf:wset-winfo-list wset)
        for wname = (car s)
        for winfo = (wlf:get-winfo wname winfo-list)
        do (wlf:window-shown-set winfo (cdr s))))

(defun wlf:maximize-window-states (wset winfo-name)
  "[internal] Set show state at the WINFO-NAME window, set hide
state at the other windows."
  (loop for winfo in (wlf:wset-winfo-list wset)
        if (eq (wlf:window-name winfo) winfo-name)
        do (wlf:window-shown-set winfo t)
        else
        do (wlf:window-shown-set winfo nil)))

(defun wlf:toggle-maximize (wset winfo-name)
  "Toggle WINFO-NAME window maximizing."
  ;; structure : (wlf:maximize maximized-winfo-name states-list)
  (let ((prev-states (wlf:maximize-info-get))
        (current-states (wlf:collect-window-states wset))
        next-states)
    (cond
     (prev-states ; now maximizing
      (cond
       ((eq winfo-name (cadr prev-states)) ; revert windows
        (wlf:revert-window-states wset (caddr prev-states)))
       (t ; maximize other window
        (setf (cadr prev-states) winfo-name)
        (setq next-states prev-states)
        (wlf:maximize-window-states wset winfo-name))))
     (t ; maximize a window
      (wlf:maximize-window-states wset winfo-name)
      (setq next-states
            (list 'wlf:maximize winfo-name current-states))))
    (wlf:layout-internal wset)
    (wlf:maximize-info-set next-states)))

(defun wlf:get-window-name (wset window)
  "Return the window name that is corresponding to the WINDOW object.
If the WINDOW is not found, return nil."
  (loop for winfo in (wlf:wset-winfo-list wset)
        for win = (wlf:window-window winfo)
        if (and win (window-live-p win)
                (eql win window))
        return (wlf:window-name winfo)))

(defun wlf:wset-live-p (wset)
  "Return t if WSET is valid, return nil otherwise."
  (let ((die-count 0) (shown-count 0))
    (loop for winfo in (wlf:wset-winfo-list wset)
          for win = (wlf:window-window winfo)
          if (wlf:window-shown-p winfo)
          do 
          (incf shown-count)
          (unless (and win (window-live-p win))
            (incf die-count)))
    (cond
     ((= die-count 0) t)
     (t (loop for winfo in (wlf:wset-winfo-list wset)
              for (left top right bottom) = (wlf:window-edges winfo)
              for sw = (window-at left top)
              with windows = nil
              do
              (when sw
                (cond
                 ((memq sw windows) (return nil))
                 (t (push sw windows))))
              finally return t)))))

(defun wlf:wset-fix-windows (wset)
  (loop for winfo in (wlf:wset-winfo-list wset)
        for win = (wlf:window-window winfo)
        do
        (cond
         ((wlf:window-shown-p winfo) ; should be displayed
          (cond
           (win ; the previous window object exists
            (let ((swin (wlf:window-window-by-edge winfo)))
              (cond
               (swin ; found a window object
                (setf (wlf:window-window winfo) swin
                      (wlf:window-edges winfo) (window-edges swin)))
               (t    ; found no window object
                (wlf:window-shown-set winfo nil)
                (setf (wlf:window-window winfo) nil
                      (wlf:window-edges winfo) nil)))))
           (t ; no previous window object
            (wlf:window-shown-set winfo nil)
            (setf (wlf:window-window winfo) nil
                  (wlf:window-edges winfo) nil))))
         (t ; not displayed
          (setf (wlf:window-window winfo) nil
                (wlf:window-edges winfo) nil)))))



;;; test

;; (setq ss
;;       (wlf:layout
;;        '(| (:left-max-size 20)
;;          folder 
;;          (- (:lower-size-ratio 0.6)
;;           summary message))
;;        '((:name folder :buffer "*info*")
;;          (:name summary :buffer "*Messages*")
;;          (:name message :buffer "window-layout.el" :default-hide nil))))

;; (setq dd
;;       (wlf:no-layout
;;        '(| (:left-size-ratio 0.33)
;;          folder 
;;          (| (:left-size-ratio 0.5)
;;           summary message))
;;        '((:name folder :buffer "*info*")
;;          (:name summary :buffer "*Messages*")
;;          (:name message :buffer "window-layout.el"))))

;; (setq ff (wlf:no-layout
;;           '(| (:left-max-size 40)
;;               (- (:upper-size-ratio 0.25)
;;                  files
;;                  (- (:upper-size-ratio 0.3)
;;                     history sub))
;;               (| (:right-max-size 30)
;;                  main imenu))
;;           '((:name main :buffer "window-layout.el")
;;             (:name files :buffer "*scratch*")
;;             (:name history :buffer "*Messages*")
;;             (:name sub :buffer "*Info*")
;;             (:name imenu :buffer "*info*" :default-hide t))))

;; (wlf:show ss 'folder)
;; (wlf:hide ss 'folder)
;; (wlf:toggle-maximize ss 'message)
;; (wlf:toggle-maximize ss 'folder)
;; (wlf:toggle ss 'folder 'summary)
;; (wlf:toggle ss 'summary)
;; (wlf:reset-window-sizes ss)
;; (wlf:select ss 'summary)
;; (wlf:get-buffer ss 'message)
;; (wlf:set-buffer ss 'message "*scratch*")
;; (wlf:refresh ss)
;; (wlf:refresh dd)
;; (wlf:refresh ff)
;; (wlf:toggle ff 'imenu)
;; (wlf:reset-window-sizes ff)

;; (wlf:get-window-name ss (selected-window))
;; (wlf:wset-live-p ss)
;; (wlf:wset-live-p ss 1)

;; (wlf:wopts-replace-buffer 
;;  '((:name folder :buffer "*info*")
;;    (:name summary :buffer "*Messages*")
;;    (:name message :buffer "window-layout.el" :default-hide nil))
;;  '((folder . "*Messages*") (summary . "*scratch*")))

;; (defun wlf:test-hook (wset) (message "HOOK : %s" wset))
;; (wlf:layout-hook-add ss 'wlf:test-hook)
;; (wlf:layout-hook-remove ss 'wlf:test-hook)

(provide 'window-layout)
;;; window-layout.el ends here
