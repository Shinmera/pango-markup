(asdf:defsystem pango-markup
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A small library to generate pango-style text markup."
  :homepage "https://shinmera.com/docs/pango-markup/"
  :bug-tracker "https://shinmera.com/project/pango-markup/issues"
  :source-control (:git "https://shinmera.com/project/pango-markup.git")
  :serial T
  :components ((:file "package")
               (:file "pango")
               (:file "documentation"))
  :depends-on (:documentation-utils))
