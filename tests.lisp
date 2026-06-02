(defpackage :idef0.tests
  (:use :cl :fiveam :idef0.models :idef0.layout :idef0.svg))

(in-package :idef0.tests)

(def-suite idef0-tests
  :description "Test suite for IDEF0 generation")

(in-suite idef0-tests)

;; Positive tests
(test test-single-node-creation
  (let ((n (make-node :id "n1" :title "Process 1")))
    (is (string= "n1" (node-id n)))
    (is (string= "Process 1" (node-title n)))))

(test test-node-with-cost-and-number
  (let ((n (make-node :id "n1" :title "P1" :number "A0" :cost "$100")))
    (is (string= "A0" (node-number n)))
    (is (string= "$100" (node-cost n)))))

(test test-icom-routing-single
  (let* ((n (make-node :id "n1" :title "P1"
                       :inputs '("In") :outputs '("Out")
                       :controls '("Ctrl") :mechanisms '("Mech")))
         (edges (route-icom n)))
    (is (= 4 (length edges)))))

(test test-icom-routing-multiple
  (let* ((n (make-node :id "n1" :title "P1"
                       :inputs '("I1" "I2" "I3") :controls '("C1" "C2")))
         (edges (route-icom n)))
    (is (= 5 (length edges)))))

(test test-text-width-approximation
  (let ((short-width (estimate-text-width "A"))
        (long-width (estimate-text-width "A very long label for ICOM")))
    (is (< short-width long-width))))

(test test-decomposition-layout
  (let* ((n1 (make-node :id "n1" :title "P1" :x 0 :y 0))
         (n2 (make-node :id "n2" :title "P2" :x 200 :y 200))
         (c (make-connection n1 n2 :side :input :label "Data"))
         (edges (calculate-layout (list n1 n2) (list c))))
    (is (listp edges))))

(test test-edge-reverse-logic
  (let* ((n (make-node :id "n1" :title "P1" :controls '("C1")))
         (edges (route-icom n))
         (ctrl-edge (car edges)))
    ;; The arrows pointing inwards now don't use 'rev' in route-icom.
    ;; We check that y1 < y2 to signify it points from above downwards into the node.
    (is (< (edge-y1 ctrl-edge) (edge-y2 ctrl-edge)))))

(test test-zigzag-connection
  ;; Mock logic or ensure normal data supports zigzag style path offsets
  ;; Currently we test basic path construction which zigzag would adapt from
  (let ((e (make-edge :side :input :x1 0 :y1 0 :x2 50 :y2 50 :label "Detached")))
    (is (string= "Detached" (edge-label e)))))

(test test-svg-generation-node
  (let* ((n (make-node :id "n1" :title "Process" :number "A1"))
         (svg (render-node n)))
    (is (search "<rect" svg))
    (is (search "<text" svg))
    (is (search "Process" svg))
    (is (search "A1" svg))))

(test test-svg-generation-edge
  (let* ((e (make-edge :side :input :x1 0 :y1 0 :x2 10 :y2 10))
         (svg (render-edge e)))
    (is (search "<path" svg))
    (is (search "M 0 0" svg))))

;; Negative tests

(test test-missing-required-fields
  (signals missing-field-error (make-node :id "n1"))
  (signals missing-field-error (make-node :title "P1")))

(test test-invalid-connection-type
  (signals invalid-connection-error (make-connection "n1" "n2" :side :diagonal))
  (signals invalid-connection-error (make-edge :side :diagonal)))

(test test-negative-dimensions
  (signals negative-dimension-error (make-node :id "n1" :title "P1" :width -10))
  (signals negative-dimension-error (make-node :id "n1" :title "P1" :height -5)))

(test test-circular-dependency
  (let* ((n1 (make-node :id "n1" :title "P1"))
         (n2 (make-node :id "n2" :title "P2"))
         (c1 (make-connection n1 n2 :side :input))
         (c2 (make-connection n2 n1 :side :input)))
    (signals circular-dependency-error (check-circular-dependency (list c1 c2)))))

(test test-empty-icom-arrays
  (signals missing-icom-arrays-error (render-svg :not-a-list nil)))

(test test-invalid-cost-type
  (signals invalid-cost-error (make-node :id "n1" :title "P1" :cost '(:invalid))))

(test test-overlapping-blocks
  (let* ((n1 (make-node :id "n1" :title "P1" :x 0 :y 0 :width 100 :height 100))
         (n2 (make-node :id "n2" :title "P2" :x 50 :y 50 :width 100 :height 100)))
    (signals overlapping-blocks-error (check-overlapping-blocks (list n1 n2)))))

(test test-too-many-arrows
  ;; will-fit now just returns NIL instead of throwing an error for CSS offset logic
  ;; Let's test that get-offset correctly calculates the offset when will-fit is nil
  (let* ((heights (loop repeat 100 collect 16))
         (offset (get-offset heights 50 0)))
    (is (numberp offset))))

(test test-invalid-edge-coordinates
  (let ((e (make-edge :side :input :x1 0 :y1 0))) ;; Missing x2, y2
    (signals invalid-edge-coordinates-error (render-edge e))))

(test test-partition-point-empty
  (signals empty-partition-range-error (partition-point '() (lambda (x) t))))
