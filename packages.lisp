(defpackage :idef0.models
  (:use :cl)
  (:export #:node #:make-node #:node-id #:node-title #:node-number #:node-cost
           #:node-inputs #:node-outputs #:node-controls #:node-mechanisms
           #:node-input-colors #:node-output-colors #:node-control-colors #:node-mechanism-colors
           #:node-width #:node-height #:node-x #:node-y
           #:edge #:make-edge #:edge-id #:edge-label #:edge-color
           #:edge-side #:edge-side-type
           #:edge-x1 #:edge-y1 #:edge-x2 #:edge-y2
           #:edge-rev
           #:connection #:make-connection #:connection-from #:connection-to #:connection-side #:connection-label
           #:model-error #:missing-field-error #:negative-dimension-error
           #:invalid-connection-error #:invalid-cost-error))

(defpackage :idef0.layout
  (:use :cl :idef0.models)
  (:export #:estimate-text-width
           #:will-fit #:get-offset #:get-label-side #:partition-point
           #:layout-error #:empty-partition-range-error #:too-many-arrows-error
           #:circular-dependency-error #:overlapping-blocks-error
           #:route-icom #:check-circular-dependency #:check-overlapping-blocks
           #:calculate-layout))

(defpackage :idef0.svg
  (:use :cl :idef0.models :idef0.layout)
  (:export #:render-svg #:render-node #:render-edge
           #:svg-error #:invalid-edge-coordinates-error
           #:missing-icom-arrays-error))

(defpackage :idef0
  (:use :cl)
  (:use :idef0.models :idef0.layout :idef0.svg)
  (:export #:generate-idef0-svg))
