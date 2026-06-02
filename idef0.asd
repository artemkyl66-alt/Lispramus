(defsystem "idef0"
  :description "IDEF0 diagram generation in Lisp"
  :version "0.1.0"
  :author "Assistant"
  :depends-on ()
  :components ((:file "packages")
               (:file "models" :depends-on ("packages"))
               (:file "layout" :depends-on ("models"))
               (:file "svg" :depends-on ("layout"))))

(defsystem "idef0/tests"
  :description "Tests for IDEF0"
  :depends-on ("idef0" "fiveam")
  :components ((:file "tests")))
