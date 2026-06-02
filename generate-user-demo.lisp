(require 'asdf)
(ql:quickload :fiveam)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :idef0)

(in-package :cl-user)

(defun generate-test-svgs ()
  (let* ((n1 (idef0.models:make-node
                :id "A0"
                :title "Закупочная деятельность предприятия"
                :width 250 :height 100
                :inputs '("Статистика продаж" "Решение о закупке руководством" "Заявка со склада" "Поставщики" "Товар от поставщика")
                :input-colors '("#1dac15" "#1dac15" "#1dac15" "#1dac15" "#1dac15")
                :outputs '("Поставленный товар" "Документы на закупленный товар")
                :output-colors '("#ff0000" "#ff0000")
                :controls '("Законодательные акты" "Конъюнктура рынка" "Метод закупки товаров")
                :mechanisms '("Сотрудники" "ПК, средства связи")
                :mechanism-colors '("#89b9b4" "#0101ae")))
         (svg1 (idef0.svg:render-svg (list n1) nil))

         (n2 (idef0.models:make-node
                :id "A1"
                :title "Закупочная деятельность предприятия"
                :width 250 :height 100
                :inputs '("Статистика продаж" "Решение о закупке руководством" "Заявка со склада" "Поставщики" "Товар от поставщика")
                :outputs '("Поставленный товар" "Документы на закупленный товар")
                :controls '("Законодательные акты" "Конъюнктура рынка" "Метод закупки товаров")
                :mechanisms '("Сотрудники" "ПК, средства связи")))
         (svg2 (idef0.svg:render-svg (list n2) nil))

         (n3 (idef0.models:make-node :id "A2" :title "Process Data" :x 0 :y 0 :width 120 :height 80 :inputs '("Raw") :outputs '("Processed")))
         (n4 (idef0.models:make-node :id "A3" :title "Store Data" :x 200 :y 100 :width 120 :height 80 :inputs '("Processed") :outputs '("Stored")))
         (c1 (idef0.models:make-connection n3 n4 :side :input :label "Transfer"))
         (svg3 (idef0.svg:render-svg (list n3 n4) (list c1))))

    (with-open-file (out "diagram1.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg1))

    (with-open-file (out "diagram2.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg2))

    (with-open-file (out "diagram3.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg3))))

(generate-test-svgs)
(quit)
