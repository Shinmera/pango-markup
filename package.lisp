#|
 This file is a part of pango-markup
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:pango-markup
  (:nicknames #:org.shirakumo.pango-markup)
  (:use #:cl)
  (:export
   #:escape-char
   #:font
   #:family
   #:size
   #:style
   #:weight
   #:variant
   #:stretch
   #:features
   #:markup
   #:font
   #:foreground
   #:background
   #:underline
   #:rise
   #:strikethrough
   #:fallback
   #:language
   #:letter-spacing
   #:gravity
   #:opening-tag
   #:closing-tag
   #:markup-regions))
