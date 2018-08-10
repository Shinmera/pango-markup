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

(defun html-escape-char (char stream)
  (case char
    (#\< (write-string "&lt;" stream))
    (#\> (write-string "&gt;" stream))
    (#\& (write-string "&amp;" stream))
    (#\' (write-string "&#39;" stream))
    (#\" (write-string "&quot;" stream))
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
        do (html-escape-char char o)))

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
  ((family :initarg :family :accessor family)
   (size :initarg :size :accessor size)
   (style :initarg :style :accessor style)
   (weight :initarg :weight :accessor weight)
   (variant :initarg :variant :accessor variant)
   (stretch :initarg :stretch :accessor stretch)
   (features :initarg :features :accessor features))
  (:default-initargs
   :family NIL
   :size :medium
   :style :normal
   :weight :normal
   :variant :normal
   :stretch :normal
   :features ()))

(defclass markup ()
  ((font :initarg :font :accessor font)
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
   :gravity :auto))

(defmethod tag ((markup markup))
  (with-slots (font foreground background underline rise
               strikethrough fallback language letter-spacing gravity)
      markup
    (with-output-to-string (o)
      (format o "<span")
      (when font
        (format o "~@[ font_family='~/pango-markup::format-escape/'~]" (family font))
        (format o "~@[ font_size='~/pango-markup::format-escape/'~]" (null-if :medium (size font)))
        (format o "~@[ font_style='~/pango-markup::format-escape/'~]" (null-if :normal (style font)))
        (format o "~@[ font_weight='~/pango-markup::format-escape/'~]" (null-if :normal (weight font)))
        (format o "~@[ font_variant='~/pango-markup::format-escape/'~]" (null-if :normal (variant font)))
        (format o "~@[ font_stretch='~/pango-markup::format-escape/'~]" (null-if :normal (stretch font)))
        (format o "~@[ font_features='~{~/pango-markup::format-escape/~^, ~}'~]" (features font)))
      (format o "~@[ color='~/pango-markup::format-color/'~]" foreground)
      (format o "~@[ background='~/pango-markup::format-color/'~]" background)
      (destructuring-bind (&optional mode color) (enlist underline)
        (format o "~@[ underline='~/pango-markup::format-escape/'~]" mode)
        (format o "~@[ underline_color='~/pango-markup::format-color/'~]" color))
      (format o "~@[ rise='~/pango-markup::format-escape/'~]" rise)
      (destructuring-bind (&optional mode color) (enlist strikethrough)
        (format o "~@[ strikethrough='~/pango-markup::format-escape/'~]" (if mode "true" NIL))
        (format o "~@[ strikethrough_color='~/pango-markup::format-color/'~]" color))
      (format o "~@[ fallback='~/pango-markup::format-escape/'~]" (if fallback NIL "false"))
      (format o "~@[ lang='~/pango-markup::format-escape/'~]" language)
      (format o "~@[ letter_spacing='~/pango-markup::format-escape/'~]" letter-spacing)
      (destructuring-bind (&optional gravity hint) (enlist gravity)
        (format o "~@[ gravity='~/pango-markup::format-escape/'~]" (null-if :auto gravity))
        (format o "~@[ gravity_hint='~/pango-markup::format-escape/'~]" hint))
      (format o ">"))))

(defmethod tag ((spec cons))
  (tag (apply #'make-instance 'markup spec)))

(defun markup-regions (text regions)
  (let ((additions (make-hash-table :test 'eql)))
    (loop for (start end . markup) in regions
          do (push (tag markup) (gethash start additions))
             (push "</span>" (gethash end additions)))
    (with-output-to-string (o)
      (loop for i from 0 below (length text)
            for char = (aref text i)
            do (format o "~{~a~}" (gethash i additions))
               (html-escape-char char o)))))
