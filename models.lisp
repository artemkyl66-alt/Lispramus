(in-package :idef0.models)

;; Error conditions
(define-condition model-error (error) ())
(define-condition missing-field-error (model-error)
  ((field :initarg :field :reader missing-field))
  (:report (lambda (condition stream)
             (format stream "Missing required field: ~A" (missing-field condition)))))
(define-condition negative-dimension-error (model-error)
  ((dimension :initarg :dimension :reader negative-dimension)
   (value :initarg :value :reader dimension-value))
  (:report (lambda (condition stream)
             (format stream "Negative dimension ~A: ~A"
                     (negative-dimension condition) (dimension-value condition)))))
(define-condition invalid-connection-error (model-error)
  ((side :initarg :side :reader invalid-side))
  (:report (lambda (condition stream)
             (format stream "Invalid connection side: ~A" (invalid-side condition)))))
(define-condition invalid-cost-error (model-error)
  ((value :initarg :value :reader invalid-cost-value))
  (:report (lambda (condition stream)
             (format stream "Invalid cost value type (expected string or number): ~A"
                     (invalid-cost-value condition)))))

;; Valid connection sides
(deftype edge-side-type () '(member :input :output :control :mechanism))

(defun validate-string-or-number (val)
  (or (stringp val) (numberp val)))

(defun validate-positive-number (val)
  (and (numberp val) (>= val 0)))

(defclass node ()
  ((id :initarg :id :accessor node-id :initform (error 'missing-field-error :field "id"))
   (title :initarg :title :accessor node-title :initform (error 'missing-field-error :field "title"))
   (number :initarg :number :accessor node-number :initform nil)
   (cost :initarg :cost :accessor node-cost :initform nil)
   (inputs :initarg :inputs :accessor node-inputs :initform nil)
   (outputs :initarg :outputs :accessor node-outputs :initform nil)
   (controls :initarg :controls :accessor node-controls :initform nil)
   (mechanisms :initarg :mechanisms :accessor node-mechanisms :initform nil)
   (input-colors :initarg :input-colors :accessor node-input-colors :initform nil)
   (output-colors :initarg :output-colors :accessor node-output-colors :initform nil)
   (control-colors :initarg :control-colors :accessor node-control-colors :initform nil)
   (mechanism-colors :initarg :mechanism-colors :accessor node-mechanism-colors :initform nil)
   (width :initarg :width :accessor node-width :initform 144) ;; default 9em at 16px
   (height :initarg :height :accessor node-height :initform 96) ;; default 6em at 16px
   ;; Location
   (x :initarg :x :accessor node-x :initform 0)
   (y :initarg :y :accessor node-y :initform 0)))

(defmethod initialize-instance :after ((n node) &key cost width height &allow-other-keys)
  (when (and cost (not (validate-string-or-number cost)))
    (error 'invalid-cost-error :value cost))
  (when (and width (not (validate-positive-number width)))
    (error 'negative-dimension-error :dimension "width" :value width))
  (when (and height (not (validate-positive-number height)))
    (error 'negative-dimension-error :dimension "height" :value height)))

(defun make-node (&rest args &key id title number cost inputs outputs controls mechanisms input-colors output-colors control-colors mechanism-colors width height x y)
  (declare (ignore id title number cost inputs outputs controls mechanisms input-colors output-colors control-colors mechanism-colors width height x y))
  (apply #'make-instance 'node args))


(defclass edge ()
  ((id :initarg :id :accessor edge-id :initform nil)
   (label :initarg :label :accessor edge-label :initform "")
   (side :initarg :side :accessor edge-side :initform :input)
   (rev :initarg :rev :accessor edge-rev :initform nil)
   (color :initarg :color :accessor edge-color :initform "black")
   (x1 :initarg :x1 :accessor edge-x1 :initform nil)
   (y1 :initarg :y1 :accessor edge-y1 :initform nil)
   (x2 :initarg :x2 :accessor edge-x2 :initform nil)
   (y2 :initarg :y2 :accessor edge-y2 :initform nil)))

(defmethod initialize-instance :after ((e edge) &key side &allow-other-keys)
  (unless (typep side 'edge-side-type)
    (error 'invalid-connection-error :side side)))

(defun make-edge (&rest args &key id label side rev color x1 y1 x2 y2)
  (declare (ignore id label side rev color x1 y1 x2 y2))
  (apply #'make-instance 'edge args))

(defclass connection ()
  ((from :initarg :from :accessor connection-from)
   (to :initarg :to :accessor connection-to)
   (side :initarg :side :accessor connection-side :initform :input)
   (label :initarg :label :accessor connection-label :initform "")))

(defmethod initialize-instance :after ((c connection) &key side &allow-other-keys)
  (unless (typep side 'edge-side-type)
    (error 'invalid-connection-error :side side)))

(defun make-connection (from to &rest args &key side label)
  (declare (ignore side label))
  (apply #'make-instance 'connection :from from :to to args))
