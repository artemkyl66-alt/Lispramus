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

         (choice (idef0.models:make-node :id "choice" :title "Выбор поставщиков" :x 0 :y 0 :width 200 :height 100))
         (order (idef0.models:make-node :id "order" :title "Формирование заказа" :x 600 :y 400 :width 200 :height 100))
         (control (idef0.models:make-node :id "control" :title "Контроль исполнения заказов" :x 1200 :y 800 :width 200 :height 100))
         (ret (idef0.models:make-node :id "return" :title "Возврат товаров поставщику" :x 1800 :y 1200 :width 200 :height 100))

         (connections (list
           ;; Ext inputs to blocks (we mock ext inputs as dummy nodes at x=-200 for inputs)
           (idef0.models:make-connection (idef0.models:make-node :id "i1" :title "" :x -400 :y 50 :width 0 :height 0) choice :side :input :label "Поставщики")
           (idef0.models:make-connection (idef0.models:make-node :id "i2" :title "" :x -400 :y 420 :width 0 :height 0) order :side :input :label "Статистика продаж")
           (idef0.models:make-connection (idef0.models:make-node :id "i3" :title "" :x -400 :y 450 :width 0 :height 0) order :side :input :label "Заявка со склада")
           (idef0.models:make-connection (idef0.models:make-node :id "i4" :title "" :x -400 :y 480 :width 0 :height 0) order :side :input :label "Решение о закупке руководством")
           (idef0.models:make-connection (idef0.models:make-node :id "i5" :title "" :x -400 :y 850 :width 0 :height 0) control :side :input :label "Товар от поставщика")

           ;; Ext controls
           (idef0.models:make-connection (idef0.models:make-node :id "c1" :title "" :x 650 :y -400 :width 0 :height 0) order :side :control :label "Законодательные акты")
           (idef0.models:make-connection (idef0.models:make-node :id "c2" :title "" :x 1250 :y -400 :width 0 :height 0) control :side :control :label "Законодательные акты")
           (idef0.models:make-connection (idef0.models:make-node :id "c3" :title "" :x 1850 :y -400 :width 0 :height 0) ret :side :control :label "Законодательные акты")
           (idef0.models:make-connection (idef0.models:make-node :id "c4" :title "" :x 50 :y -400 :width 0 :height 0) choice :side :control :label "Конъюнктура рынка")
           (idef0.models:make-connection (idef0.models:make-node :id "c5" :title "" :x 750 :y -400 :width 0 :height 0) order :side :control :label "Конъюнктура рынка")
           (idef0.models:make-connection (idef0.models:make-node :id "c6" :title "" :x 150 :y -400 :width 0 :height 0) choice :side :control :label "Метод закупки товаров")
           (idef0.models:make-connection (idef0.models:make-node :id "c7" :title "" :x 850 :y -400 :width 0 :height 0) order :side :control :label "Метод закупки товаров")

           ;; Ext mechanisms
           (idef0.models:make-connection (idef0.models:make-node :id "m1" :title "" :x 50 :y 1600 :width 0 :height 0) choice :side :mechanism :label "Сотрудники")
           (idef0.models:make-connection (idef0.models:make-node :id "m2" :title "" :x 650 :y 1600 :width 0 :height 0) order :side :mechanism :label "Сотрудники")
           (idef0.models:make-connection (idef0.models:make-node :id "m3" :title "" :x 1250 :y 1600 :width 0 :height 0) control :side :mechanism :label "Сотрудники")
           (idef0.models:make-connection (idef0.models:make-node :id "m4" :title "" :x 1850 :y 1600 :width 0 :height 0) ret :side :mechanism :label "Сотрудники")
           (idef0.models:make-connection (idef0.models:make-node :id "m5" :title "" :x 150 :y 1600 :width 0 :height 0) choice :side :mechanism :label "ПК, средства связи")
           (idef0.models:make-connection (idef0.models:make-node :id "m6" :title "" :x 750 :y 1600 :width 0 :height 0) order :side :mechanism :label "ПК, средства связи")
           (idef0.models:make-connection (idef0.models:make-node :id "m7" :title "" :x 1350 :y 1600 :width 0 :height 0) control :side :mechanism :label "ПК, средства связи")
           (idef0.models:make-connection (idef0.models:make-node :id "m8" :title "" :x 1950 :y 1600 :width 0 :height 0) ret :side :mechanism :label "ПК, средства связи")

           ;; Output nodes
           (idef0.models:make-connection control (idef0.models:make-node :id "o1" :title "" :x 2400 :y 800 :width 0 :height 0) :side :output :label "Документы на закупленный товар")
           (idef0.models:make-connection control (idef0.models:make-node :id "o2" :title "" :x 2400 :y 850 :width 0 :height 0) :side :output :label "Поставленный товар")
           (idef0.models:make-connection ret (idef0.models:make-node :id "o3" :title "" :x 2400 :y 1250 :width 0 :height 0) :side :output :label "Поставленный товар")

           ;; Block to Block connections
           (idef0.models:make-connection choice order :side :input :label "Список поставщиков")
           (idef0.models:make-connection order order :side :control :label "График выполнения поставок")
           (idef0.models:make-connection order control :side :control :label "График выполнения поставок")
           (idef0.models:make-connection order control :side :control :label "Договор")
           (idef0.models:make-connection order ret :side :control :label "Договор")
           (idef0.models:make-connection order control :side :input :label "Заказ")
           (idef0.models:make-connection order control :side :input :label "Требование подтверждения")
           (idef0.models:make-connection control order :side :input :label "Отказ принятия заказа")
           (idef0.models:make-connection control ret :side :input :label "Неудовлетворяющий требованиям товар")
         ))
         (svg3 (idef0.svg:render-svg (list choice order control ret) connections)))

    (with-open-file (out "diagram1.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg1))

    (with-open-file (out "diagram2.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg2))

    (with-open-file (out "diagram3.svg" :direction :output :if-exists :supersede)
      (format out "~A" svg3))))

(generate-test-svgs)
(quit)
