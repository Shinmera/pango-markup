#|
 This file is a part of pango-markup
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.pango-markup)

(defun enlist (list &rest items)
  (if (listp list) list (list* list items)))

(defun null-if (nullable item)
  (if (eql nullable item) NIL item))

(defun escape-char (char stream)
  (case char
    (#\< (write-string "&lt;" stream))
    (#\> (write-string "&gt;" stream))
    (#\& (write-string "&amp;" stream))
    (#\' (write-string "&#39;" stream))
    (T (write-char char stream))))

(defmethod format-escape (o thing &rest args)
  (declare (ignore args))
  (format-escape o (princ-to-string thing)))

(defmethod format-escape (o (symbol symbol) &rest args)
  (declare (ignore args))
  (format-escape o (string-downcase symbol)))

(defmethod format-escape (o (string string) &rest args)
  (declare (ignore args))
  (loop for char across string
        do (escape-char char o)))

(defmethod format-color (o color &rest args)
  (declare (ignore args))
  (format-escape o color))

(defmethod format-color (o (color integer) &rest args)
  (declare (ignore args))
  (format o "#~6,'0x" color))

(defmethod format-color (o (color list) &rest args)
  (declare (ignore args))
  (apply #'format o "#~2,'0x~2,'0x~2,'0x~@[~2,'0x~]" color))

(defclass font ()
  ((family :initarg :family :accessor family :type (or null string))
   (size :initarg :size :accessor size :type (or null real (member :xx-small :x-small :small :medium :large :x-larg :xx-large :smaller :larger)))
   (style :initarg :style :accessor style :type (or null (member :normal :oblique :italic)))
   (weight :initarg :weight :accessor weight :type (or null real (member :ultra-light :light :normal :bold :ultra-bold :heavy)))
   (variant :initarg :variant :accessor variant :type (or null (member :normal :small-caps)))
   (stretch :initarg :stretch :accessor stretch :type (or null (member :ultra-condensed :extra-condensed :condensed :semi-condensed :normal :semi-expanded :expanded :extra-expanded :ultra-expanded)))
   (features :initarg :features :accessor features))
  (:default-initargs
   :family NIL
   :size NIL
   :style NIL
   :weight NIL
   :variant NIL
   :stretch NIL
   :features ()))

(defclass markup ()
  ((font :initform NIL :accessor font)
   (foreground :initarg :foreground :initarg :color :accessor foreground)
   (background :initarg :background :accessor background)
   (underline :initarg :underline :accessor underline)
   (rise :initarg :rise :accessor rise)
   (strikethrough :initarg :strikethrough :initarg :strike :accessor strikethrough)
   (fallback :initarg :fallback :accessor fallback)
   (language :initarg :language :initarg :lang :accessor language)
   (letter-spacing :initarg :letter-spacing :initarg :spacing :accessor letter-spacing)
   (gravity :initarg :gravity :accessor gravity))
  (:default-initargs
   :font NIL
   :foreground NIL
   :background NIL
   :underline NIL
   :rise NIL
   :strikethrough NIL
   :fallback T
   :language NIL
   :letter-spacing NIL
   :gravity NIL))

(defmethod shared-initialize :after ((markup markup) slots &key font)
  (typecase font
    (font (setf (font markup) font))
    (cons (setf (font markup) (apply #'make-instance 'font font)))))

(defmethod tag ((markup markup))
  (with-slots (font foreground background underline rise
               strikethrough fallback language letter-spacing gravity)
      markup
    (with-output-to-string (o)
      (format o "<span")
      (when font
        (format o "~@[ font_family='~/pango-markup::format-escape/'~]" (family font))
        (format o "~@[ font_size='~/pango-markup::format-escape/'~]"
                (etypecase (size font)
                  (null NIL)
                  (real (round (* 1024 (size font))))
                  (keyword (size font))))
        (format o "~@[ font_style='~a'~]"
                (ecase (style font)
                  ((NIL))
                  (:normal "normal")
                  (:oblique "oblique")
                  (:italic "italic")))
        (format o "~@[ font_weight='~a'~]"
                (ecase (weight font)
                  ((NIL))
                  (:ultra-light "ultralight")
                  (:light "light")
                  (:normal "normal")
                  (:bold "bold")
                  (:ultra-bold "ultrabold")
                  (:heavy "heavy")))
        (format o "~@[ font_variant='~a'~]"
                (ecase (variant font)
                  ((NIL))
                  (:normal "normal")
                  (:small-caps "smallcaps")))
        (format o "~@[ font_stretch='~a'~]"
                (ecase (stretch font)
                  ((NIL))
                  (:ultra-condensed "ultracondensed")
                  (:extra-condensed "extracondensed")
                  (:condensed "condensed")
                  (:semi-condensed "semicondensed")
                  (:normal "normal")
                  (:semi-expanded "semiexpanded")
                  (:expanded "expanded")
                  (:extra-expanded "extraexpanded")
                  (:ultra-expanded "ultraexpanded")))
        (format o "~@[ font_features='~{~/pango-markup::format-escape/~^, ~}'~]" (features font)))
      (format o "~@[ color='~/pango-markup::format-color/'~]" foreground)
      (format o "~@[ background='~/pango-markup::format-color/'~]" background)
      (destructuring-bind (&optional mode color) (enlist underline)
        (format o "~@[ underline='~a'~]"
                (ecase mode
                  ((NIL))
                  (:none "none")
                  ((:single T) "single")
                  (:double "double")
                  (:low "low")
                  (:error "error")))
        (format o "~@[ underline_color='~/pango-markup::format-color/'~]" color))
      (format o "~@[ rise='~/pango-markup::format-escape/'~]" rise)
      (destructuring-bind (&optional mode color) (enlist strikethrough)
        (format o "~@[ strikethrough='~a'~]" (if mode "true" NIL))
        (format o "~@[ strikethrough_color='~/pango-markup::format-color/'~]" color))
      (format o "~@[ fallback='~a'~]" (if fallback NIL "false"))
      (format o "~@[ lang='~/pango-markup::format-escape/'~]" language)
      (format o "~@[ letter_spacing='~a'~]"
              (etypecase letter-spacing
                (null NIL)
                (real (round (* 1024 letter-spacing)))))
      (destructuring-bind (&optional gravity hint) (enlist gravity)
        (format o "~@[ gravity='~a'~]"
                (ecase gravity
                  ((NIL))
                  (:south ":outh")
                  (:east "east")
                  (:north ":orth")
                  (:west "west")
                  (:auto "auto")))
        (format o "~@[ gravity_hint='~/pango-markup::format-escape/'~]"
                (ecase hint
                  ((NIL))
                  (:natural "natural")
                  (:strong "strong")
                  (:line "line"))))
      (format o ">"))))

(defmethod opening-tag ((spec cons))
  (tag (apply #'make-instance 'markup spec)))

(defmethod closing-tag (markup)
  "</span>")

(defun markup-regions (text regions)
  (let ((additions (make-hash-table :test 'eql)))
    (loop for (start end . markup) in regions
          do (push (opening-tag markup) (gethash start additions))
             (push (closing-tag markup) (gethash end additions)))
    (with-output-to-string (o)
      (loop for i from 0 below (length text)
            for char = (aref text i)
            do ;; The sort is necessary to ensure that closing tags come first.
               (format o "~{~a~}" (sort (gethash i additions) #'string<))
               (escape-char char o)
            finally (format o "~{~a~}" (gethash (length text) additions))))))
