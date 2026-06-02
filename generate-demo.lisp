(require 'asdf)
(ql:quickload :fiveam)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :idef0)

(in-package :cl-user)

(defun generate-test-svgs ()
  (let* ((n1 (idef0.models:make-node :id "A0" :title "Manage System" :inputs '("Data") :outputs '("Result") :controls '("Rules") :mechanisms '("Admin")))
         (svg1 (idef0.svg:render-svg (list n1) nil))

         (n2 (idef0.models:make-node :id "A1" :title "Process Data" :x 0 :y 0 :width 120 :height 80 :inputs '("Raw") :outputs '("Processed")))
         (n3 (idef0.models:make-node :id "A2" :title "Store Data" :x 200 :y 100 :width 120 :height 80 :inputs '("Processed") :outputs '("Stored")))
         (c1 (idef0.models:make-connection n2 n3 :side :input :label "Transfer"))
         (svg2 (idef0.svg:render-svg (list n2 n3) (list c1)))

         (n4 (idef0.models:make-node :id "A3" :title "Report" :x 0 :y 0 :controls '("Schedule") :mechanisms '("Engine") :outputs '("PDF")))
         (svg3 (idef0.svg:render-svg (list n4) nil)))

    (with-open-file (out "final_diagram.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg2))

    (with-open-file (out "diagram1.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg1))

    (with-open-file (out "diagram3.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg3))))

(generate-test-svgs)
(quit)
