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
              nil)))))

(defun get-offset (label-min-size-list side i)
  "Calculate CSS's space-evenly style offsets."
  (let ((total (length label-min-size-list)))
    (when (or (= total 0) (= total 1))
      (return-from get-offset 0))
    (let ((fits (will-fit label-min-size-list side)))
      (if fits
          (let* ((need-space (+ (reduce #'+ (butlast label-min-size-list) :initial-value 0)
                                (* *line-stroke-thickness* total)
                                (* *label-padding* total)))
                 (even-spacing (/ (- side need-space) (1+ total)))
                 (left-start (- (/ side 2)))
                 (offset (+ (* (+ even-spacing *line-stroke-thickness* *label-padding*) i)
                            (reduce #'+ (subseq label-min-size-list 0 i) :initial-value 0)
                            even-spacing
                            (/ *line-stroke-thickness* 2))))
            (+ left-start offset))
          (let* ((need-space (+ (reduce #'+ (rest (butlast label-min-size-list)) :initial-value 0)
                                (* *line-stroke-thickness* total)
                                (* *label-padding* total)))
                 (even-spacing (/ (- side need-space) (1+ total)))
                 (left-start (- (/ side 2)))
                 (offset (+ (* (+ even-spacing *line-stroke-thickness* *label-padding*) i)
                            (if (>= i 1)
                                (reduce #'+ (subseq label-min-size-list 1 i) :initial-value 0)
                                0)
                            even-spacing
                            (/ *line-stroke-thickness* 2))))
            (+ left-start offset))))))

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
           (heights (mapcar #'estimate-text-height inputs))
           (colors (node-input-colors node)))
      (loop for i from 0 below n
            for label in inputs
            for color = (if colors (nth i colors) "black")
            do (let* ((offset (get-offset heights height i))
                      (ey (+ ny (/ height 2) offset))
                      (ex nx))
                 (push (make-edge :label label :side :input :color (or color "black") :x1 (- ex 50) :y1 ey :x2 ex :y2 ey) edges))))

    ;; Outputs (right side, distributed vertically)
    (let* ((outputs (node-outputs node))
           (n (length outputs))
           (heights (mapcar #'estimate-text-height outputs))
           (colors (node-output-colors node)))
      (loop for i from 0 below n
            for label in outputs
            for color = (if colors (nth i colors) "black")
            do (let* ((offset (get-offset heights height i))
                      (ey (+ ny (/ height 2) offset))
                      (ex (+ nx width)))
                 (push (make-edge :label label :side :output :color (or color "black") :x1 ex :y1 ey :x2 (+ ex 50) :y2 ey) edges))))

    ;; Controls (top side, distributed horizontally)
    (let* ((controls (node-controls node))
           (n (length controls))
           (widths (mapcar #'estimate-text-width controls))
           (colors (node-control-colors node)))
      (loop for i from 0 below n
            for label in controls
            for color = (if colors (nth i colors) "black")
            do (let* ((offset (get-offset widths width i))
                      (ex (+ nx (/ width 2) offset))
                      (ey ny))
                 ;; Arrows should point *into* the node (y1 -> y2)
                 (push (make-edge :label label :side :control :color (or color "black") :x1 ex :y1 (- ey 50) :x2 ex :y2 ey) edges))))

    ;; Mechanisms (bottom side, distributed horizontally)
    (let* ((mechanisms (node-mechanisms node))
           (n (length mechanisms))
           (widths (mapcar #'estimate-text-width mechanisms))
           (colors (node-mechanism-colors node)))
      (loop for i from 0 below n
            for label in mechanisms
            for color = (if colors (nth i colors) "black")
            do (let* ((offset (get-offset widths width i))
                      (ex (+ nx (/ width 2) offset))
                      (ey (+ ny height)))
                 ;; Arrows should point *into* the node (y1 -> y2)
                 (push (make-edge :label label :side :mechanism :color (or color "black") :x1 ex :y1 (+ ey 50) :x2 ex :y2 ey) edges))))
    edges))

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
  ;; Feedback loops are normal in IDEF0, so circular dependencies are allowed.
  (check-overlapping-blocks nodes)
  (let ((all-edges nil)
        (from-counts (make-hash-table))
        (to-counts (make-hash-table :test 'equal)))

    (dolist (node nodes)
      (setf all-edges (append all-edges (route-icom node))))

    (dolist (c connections)
      (incf (gethash (connection-from c) from-counts 0))
      (incf (gethash (list (connection-to c) (connection-side c)) to-counts 0)))

    (let ((from-seen (make-hash-table))
          (to-seen (make-hash-table :test 'equal))
          (y-track 10))
      (dolist (c connections)
        (let* ((n1 (connection-from c))
               (n2 (connection-to c))
               (label (connection-label c))
               (side (connection-side c))

               (f-idx (gethash n1 from-seen 0))
               (f-total (gethash n1 from-counts 1))
               (f-offset (if (> f-total 1) (- (* (/ (node-height n1) f-total) f-idx) (/ (node-height n1) 2.5)) 0))

               (t-idx (gethash (list n2 side) to-seen 0))
               (t-total (gethash (list n2 side) to-counts 1))

               (x1 (+ (node-x n1) (node-width n1)))
               (y1 (+ (node-y n1) (/ (node-height n1) 2) f-offset)))

          (setf (gethash n1 from-seen) (1+ f-idx))
          (setf (gethash (list n2 side) to-seen) (1+ t-idx))

          (case side
            (:input
             (let* ((t-offset (if (> t-total 1) (- (* (/ (node-height n2) t-total) t-idx) (/ (node-height n2) 2.5)) 0))
                    (x2 (node-x n2))
                    (y2 (+ (node-y n2) (/ (node-height n2) 2) t-offset)))
               (if (<= x1 x2)
                   ;; Forward
                   (let ((mid-x (+ x1 (/ (- x2 x1) 2))))
                     (push (make-edge :label label :side :input :x1 x1 :y1 y1 :x2 mid-x :y2 y1) all-edges)
                     (push (make-edge :label "" :side :input :x1 mid-x :y1 y1 :x2 mid-x :y2 y2) all-edges)
                     (push (make-edge :label "" :side :input :rev t :x1 mid-x :y1 y2 :x2 x2 :y2 y2) all-edges))
                   ;; Feedback (right to left)
                   (let ((mid-y (+ (max y1 y2) 40 y-track)))
                     (incf y-track 15)
                     (push (make-edge :label label :side :input :x1 x1 :y1 y1 :x2 (+ x1 20) :y2 y1) all-edges)
                     (push (make-edge :label "" :side :input :x1 (+ x1 20) :y1 y1 :x2 (+ x1 20) :y2 mid-y) all-edges)
                     (push (make-edge :label "" :side :input :x1 (+ x1 20) :y1 mid-y :x2 (- x2 20) :y2 mid-y) all-edges)
                     (push (make-edge :label "" :side :input :x1 (- x2 20) :y1 mid-y :x2 (- x2 20) :y2 y2) all-edges)
                     (push (make-edge :label "" :side :input :rev t :x1 (- x2 20) :y1 y2 :x2 x2 :y2 y2) all-edges)))))
            (:control
             (let* ((t-offset (if (> t-total 1) (- (* (/ (node-width n2) t-total) t-idx) (/ (node-width n2) 2.5)) 0))
                    (x2 (+ (node-x n2) (/ (node-width n2) 2) t-offset))
                    (y2 (node-y n2)))
               (if (eq n1 n2)
                   ;; Self loop
                   (let ((mid-x (+ x1 30 y-track))
                         (mid-y (- y2 30 y-track)))
                     (incf y-track 15)
                     (push (make-edge :label label :side :control :x1 x1 :y1 y1 :x2 mid-x :y2 y1) all-edges)
                     (push (make-edge :label "" :side :control :x1 mid-x :y1 y1 :x2 mid-x :y2 mid-y) all-edges)
                     (push (make-edge :label "" :side :control :x1 mid-x :y1 mid-y :x2 x2 :y2 mid-y) all-edges)
                     (push (make-edge :label "" :side :control :rev t :x1 x2 :y1 mid-y :x2 x2 :y2 y2) all-edges))
                   (if (<= x1 x2)
                       ;; Forward
                       (let ((mid-x x2))
                         (push (make-edge :label label :side :control :x1 x1 :y1 y1 :x2 mid-x :y2 y1) all-edges)
                         (push (make-edge :label "" :side :control :rev t :x1 mid-x :y1 y1 :x2 x2 :y2 y2) all-edges))
                       ;; Feedback
                       (let ((mid-y (- (min y1 y2) 40 y-track)))
                         (incf y-track 15)
                         (push (make-edge :label label :side :control :x1 x1 :y1 y1 :x2 (+ x1 20) :y2 y1) all-edges)
                         (push (make-edge :label "" :side :control :x1 (+ x1 20) :y1 y1 :x2 (+ x1 20) :y2 mid-y) all-edges)
                         (push (make-edge :label "" :side :control :x1 (+ x1 20) :y1 mid-y :x2 x2 :y2 mid-y) all-edges)
                         (push (make-edge :label "" :side :control :rev t :x1 x2 :y1 mid-y :x2 x2 :y2 y2) all-edges))))))
            (:mechanism
             (let* ((t-offset (if (> t-total 1) (- (* (/ (node-width n2) t-total) t-idx) (/ (node-width n2) 2.5)) 0))
                    (x2 (+ (node-x n2) (/ (node-width n2) 2) t-offset))
                    (y2 (+ (node-y n2) (node-height n2))))
               (if (<= x1 x2)
                   ;; Forward
                   (let ((mid-x x2))
                     (push (make-edge :label label :side :mechanism :x1 x1 :y1 y1 :x2 mid-x :y2 y1) all-edges)
                     (push (make-edge :label "" :side :mechanism :rev t :x1 mid-x :y1 y1 :x2 x2 :y2 y2) all-edges))
                   ;; Feedback
                   (let ((mid-y (+ (max y1 y2) 40 y-track)))
                     (incf y-track 15)
                     (push (make-edge :label label :side :mechanism :x1 x1 :y1 y1 :x2 (+ x1 20) :y2 y1) all-edges)
                     (push (make-edge :label "" :side :mechanism :x1 (+ x1 20) :y1 y1 :x2 (+ x1 20) :y2 mid-y) all-edges)
                     (push (make-edge :label "" :side :mechanism :x1 (+ x1 20) :y1 mid-y :x2 x2 :y2 mid-y) all-edges)
                     (push (make-edge :label "" :side :mechanism :rev t :x1 x2 :y1 mid-y :x2 x2 :y2 y2) all-edges)))))
            (:output
             (push (make-edge :label label :side :output :x1 x1 :y1 y1 :x2 (+ x1 50) :y2 y1) all-edges))))))
    all-edges))
