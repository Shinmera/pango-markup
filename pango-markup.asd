(asdf:defsystem pango-markup
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A small library to generate pango-style text markup."
  :homepage "https://Shinmera.github.io/pango-markup/"
  :bug-tracker "https://github.com/Shinmera/pango-markup/issues"
  :source-control (:git "https://github.com/Shinmera/pango-markup.git")
  :serial T
  :components ((:file "package")
               (:file "pango")
               (:file "documentation"))
  :depends-on (:documentation-utils))
