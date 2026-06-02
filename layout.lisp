(in-package :idef0.layout)

;; Error conditions
(define-condition layout-error (error) ())
(define-condition empty-partition-range-error (layout-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Empty range in partition-point"))))
(define-condition too-many-arrows-error (layout-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Too many arrows to fit on the side"))))
(define-condition circular-dependency-error (layout-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Circular dependency detected"))))
(define-condition overlapping-blocks-error (layout-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Overlapping blocks detected"))))

(defparameter *char-width-px* 8)
(defparameter *line-stroke-thickness* 1)
(defparameter *label-padding* 3.2) ;; equivalent to 0.2em for 16px

(defun estimate-text-width (text)
  "Heuristic assessment of text length. e.g. 1 char = 8px"
  (if text
      (* (length text) *char-width-px*)
      0))

(defun estimate-text-height (text)
  (if text 16 0))

(defun will-fit (label-min-size-list side)
  "Checks if items can fit into a given side length"
  (let ((total (length label-min-size-list)))
    (when (= total 0) (return-from will-fit t))
    (if (= total 1)
        t
        (let* ((sum-except-last (reduce #'+ (butlast label-min-size-list) :initial-value 0))
               (need-space (+ sum-except-last
                              (* *line-stroke-thickness* total)
                              (* *label-padding* total)))
               (readable-space (* *label-padding* (1- total))))
          (if (<= (+ need-space readable-space) side)
              t
              (error 'too-many-arrows-error))))))

(defun get-offset (label-min-size-list side i)
  "Calculate CSS's space-evenly style offsets."
  (let ((total (length label-min-size-list)))
    (when (or (= total 0) (= total 1))
      (return-from get-offset 0))
    (will-fit label-min-size-list side) ;; will error if it doesn't fit
    (let* ((need-space (+ (reduce #'+ (butlast label-min-size-list) :initial-value 0)
                          (* *line-stroke-thickness* total)
                          (* *label-padding* total)))
           (even-spacing (/ (- side need-space) (1+ total)))
           (left-start (- (/ side 2)))
           (offset (+ (* (+ even-spacing *line-stroke-thickness* *label-padding*) i)
                      (reduce #'+ (subseq label-min-size-list 0 i) :initial-value 0)
                      even-spacing
                      (/ *line-stroke-thickness* 2))))
      (+ left-start offset))))

(defun get-label-side (label-min-size-list side i input-p)
  (let ((total (length label-min-size-list)))
    (when (or (= total 0) (= total 1))
      (return-from get-label-side (if input-p :left :right)))
    (if input-p :left :right)))

(defun partition-point (sequence func)
  "Return index of partition point where func transitions from returning true to false."
  (let ((size (length sequence)))
    (when (= size 0)
      (error 'empty-partition-range-error))
    (let ((base 0))
      (loop while (> size 1) do
        (let* ((half (floor size 2))
               (mid (+ base half))
               (lte (funcall func (elt sequence mid))))
          (if lte
              (setf base mid)
              (setf base base))
          (decf size half)))
      (let ((lte (funcall func (elt sequence base))))
        (if lte (1+ base) base)))))

(defun route-icom (node)
  "Calculate and return coordinates for node edges based on spacing logic."
  (let ((edges nil)
        (width (node-width node))
        (height (node-height node))
        (nx (node-x node))
        (ny (node-y node)))

    ;; Inputs (left side, distributed vertically)
    (let* ((inputs (node-inputs node))
           (n (length inputs))
           (heights (mapcar #'estimate-text-height inputs)))
      (loop for i from 0 below n
            for label in inputs
            do (let* ((offset (get-offset heights height i))
                      (ey (+ ny (/ height 2) offset))
                      (ex nx))
                 (push (make-edge :label label :side :input :x1 (- ex 50) :y1 ey :x2 ex :y2 ey) edges))))

    ;; Outputs (right side, distributed vertically)
    (let* ((outputs (node-outputs node))
           (n (length outputs))
           (heights (mapcar #'estimate-text-height outputs)))
      (loop for i from 0 below n
            for label in outputs
            do (let* ((offset (get-offset heights height i))
                      (ey (+ ny (/ height 2) offset))
                      (ex (+ nx width)))
                 (push (make-edge :label label :side :output :x1 ex :y1 ey :x2 (+ ex 50) :y2 ey) edges))))

    ;; Controls (top side, distributed horizontally)
    (let* ((controls (node-controls node))
           (n (length controls))
           (widths (mapcar #'estimate-text-width controls)))
      (loop for i from 0 below n
            for label in controls
            do (let* ((offset (get-offset widths width i))
                      (ex (+ nx (/ width 2) offset))
                      (ey ny))
                 (push (make-edge :label label :side :control :rev t :x1 ex :y1 (- ey 50) :x2 ex :y2 ey) edges))))

    ;; Mechanisms (bottom side, distributed horizontally)
    (let* ((mechanisms (node-mechanisms node))
           (n (length mechanisms))
           (widths (mapcar #'estimate-text-width mechanisms)))
      (loop for i from 0 below n
            for label in mechanisms
            do (let* ((offset (get-offset widths width i))
                      (ex (+ nx (/ width 2) offset))
                      (ey (+ ny height)))
                 (push (make-edge :label label :side :mechanism :rev t :x1 ex :y1 (+ ey 50) :x2 ex :y2 ey) edges))))
    edges))

(defun check-circular-dependency (connections)
  "Basic circular dependency checker for decomposition nodes.
connections is a list of make-connection."
  (let ((adj (make-hash-table :test 'equal)))
    (dolist (c connections)
      (push (connection-to c) (gethash (connection-from c) adj)))

    (let ((visited (make-hash-table :test 'equal))
          (rec-stack (make-hash-table :test 'equal)))
      (labels ((dfs (v)
                 (when (gethash v rec-stack)
                   (return-from dfs t))
                 (when (gethash v visited)
                   (return-from dfs nil))
                 (setf (gethash v visited) t)
                 (setf (gethash v rec-stack) t)
                 (dolist (neighbor (gethash v adj))
                   (when (dfs neighbor)
                     (return-from dfs t)))
                 (setf (gethash v rec-stack) nil)
                 nil))
        (dolist (c connections)
          (when (dfs (connection-from c))
            (error 'circular-dependency-error)))))))

(defun check-overlapping-blocks (nodes)
  "Checks if any two rectangular nodes overlap."
  (let ((n (length nodes)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let ((n1 (nth i nodes))
              (n2 (nth j nodes)))
          (unless (or (<= (+ (node-x n1) (node-width n1)) (node-x n2))
                      (>= (node-x n1) (+ (node-x n2) (node-width n2)))
                      (<= (+ (node-y n1) (node-height n1)) (node-y n2))
                      (>= (node-y n1) (+ (node-y n2) (node-height n2))))
            (error 'overlapping-blocks-error)))))))

(defun calculate-layout (nodes connections)
  (check-circular-dependency connections)
  (check-overlapping-blocks nodes)
  (let ((all-edges nil))
    (dolist (node nodes)
      (setf all-edges (append all-edges (route-icom node))))
    all-edges))
