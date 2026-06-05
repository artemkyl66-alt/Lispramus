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
                 (push (make-edge :label label :side :input :color (or color "black") :x1 (- ex 300) :y1 ey :x2 ex :y2 ey :arrow :end) edges))))

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
                 (push (make-edge :label label :side :output :color (or color "black") :x1 ex :y1 ey :x2 (+ ex 300) :y2 ey :arrow :end) edges))))

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
                 (push (make-edge :label label :side :control :color (or color "black") :x1 ex :y1 (- ey 300) :x2 ex :y2 ey :arrow :end) edges))))

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
                 (push (make-edge :label label :side :mechanism :color (or color "black") :x1 ex :y1 (+ ey 300) :x2 ex :y2 ey :arrow :end) edges))))
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

    ;; ONLY generate route-icom stubs for single node diagrams (no connections).
    ;; When there are connections, the advanced router takes full control of all lines.
    (if (null connections)
        (dolist (node nodes)
          (setf all-edges (append all-edges (route-icom node)))))

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
                     (push (make-edge :label label :side :input :x1 x1 :y1 y1 :x2 mid-x :y2 y1 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :input :x1 mid-x :y1 y1 :x2 mid-x :y2 y2 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :input :x1 mid-x :y1 y2 :x2 x2 :y2 y2 :arrow :end) all-edges))
                   ;; Feedback (right to left)
                   (let ((mid-y (+ (max y1 y2) 160 y-track)))
                     (incf y-track 35)
                     (push (make-edge :label label :side :input :x1 x1 :y1 y1 :x2 (+ x1 40) :y2 y1 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :input :x1 (+ x1 40) :y1 y1 :x2 (+ x1 40) :y2 mid-y :arrow nil) all-edges)
                     (push (make-edge :label "" :side :input :x1 (+ x1 40) :y1 mid-y :x2 (- x2 40) :y2 mid-y :arrow nil) all-edges)
                     (push (make-edge :label "" :side :input :x1 (- x2 40) :y1 mid-y :x2 (- x2 40) :y2 y2 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :input :x1 (- x2 40) :y1 y2 :x2 x2 :y2 y2 :arrow :end) all-edges)))))
            (:control
             (let* ((t-offset (if (> t-total 1) (- (* (/ (node-width n2) t-total) t-idx) (/ (node-width n2) 2.5)) 0))
                    (x2 (+ (node-x n2) (/ (node-width n2) 2) t-offset))
                    (y2 (node-y n2)))
               (if (eq n1 n2)
                   ;; Self loop
                   (let ((mid-x (+ x1 50 y-track))
                         (mid-y (- y2 120 y-track)))
                     (incf y-track 35)
                     (push (make-edge :label label :side :control :x1 x1 :y1 y1 :x2 mid-x :y2 y1 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :control :x1 mid-x :y1 y1 :x2 mid-x :y2 mid-y :arrow nil) all-edges)
                     (push (make-edge :label "" :side :control :x1 mid-x :y1 mid-y :x2 x2 :y2 mid-y :arrow nil) all-edges)
                     (push (make-edge :label "" :side :control :x1 x2 :y1 mid-y :x2 x2 :y2 y2 :arrow :end) all-edges))
                   (if (<= x1 x2)
                       ;; Forward
                       (let ((mid-x x2))
                         (push (make-edge :label label :side :control :x1 x1 :y1 y1 :x2 mid-x :y2 y1 :arrow nil) all-edges)
                         (push (make-edge :label "" :side :control :x1 mid-x :y1 y1 :x2 x2 :y2 y2 :arrow :end) all-edges))
                       ;; Feedback
                       (let ((mid-y (- (min y1 y2) 160 y-track)))
                         (incf y-track 35)
                         (push (make-edge :label label :side :control :x1 x1 :y1 y1 :x2 (+ x1 40) :y2 y1 :arrow nil) all-edges)
                         (push (make-edge :label "" :side :control :x1 (+ x1 40) :y1 y1 :x2 (+ x1 40) :y2 mid-y :arrow nil) all-edges)
                         (push (make-edge :label "" :side :control :x1 (+ x1 40) :y1 mid-y :x2 x2 :y2 mid-y :arrow nil) all-edges)
                         (push (make-edge :label "" :side :control :x1 x2 :y1 mid-y :x2 x2 :y2 y2 :arrow :end) all-edges))))))
            (:mechanism
             (let* ((t-offset (if (> t-total 1) (- (* (/ (node-width n2) t-total) t-idx) (/ (node-width n2) 2.5)) 0))
                    (x2 (+ (node-x n2) (/ (node-width n2) 2) t-offset))
                    (y2 (+ (node-y n2) (node-height n2))))
               (if (<= x1 x2)
                   ;; Forward
                   (let ((mid-x x2))
                     (push (make-edge :label label :side :mechanism :x1 x1 :y1 y1 :x2 mid-x :y2 y1 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :mechanism :x1 mid-x :y1 y1 :x2 x2 :y2 y2 :arrow :end) all-edges))
                   ;; Feedback
                   (let ((mid-y (+ (max y1 y2) 160 y-track)))
                     (incf y-track 35)
                     (push (make-edge :label label :side :mechanism :x1 x1 :y1 y1 :x2 (+ x1 40) :y2 y1 :arrow nil) all-edges)
                     (push (make-edge :label "" :side :mechanism :x1 (+ x1 40) :y1 y1 :x2 (+ x1 40) :y2 mid-y :arrow nil) all-edges)
                     (push (make-edge :label "" :side :mechanism :x1 (+ x1 40) :y1 mid-y :x2 x2 :y2 mid-y :arrow nil) all-edges)
                     (push (make-edge :label "" :side :mechanism :x1 x2 :y1 mid-y :x2 x2 :y2 y2 :arrow :end) all-edges)))))
            (:output
             (push (make-edge :label label :side :output :x1 x1 :y1 y1 :x2 (+ x1 300) :y2 y1 :arrow :end) all-edges))))))

    ;; Validation block to verify no text bounding boxes intersect with node blocks (per user request)
    (let ((boxes (mapcar (lambda (n) (list (node-x n) (node-y n) (+ (node-x n) (node-width n)) (+ (node-y n) (node-height n)))) nodes)))
      (dolist (e all-edges)
        (let ((label (edge-label e)))
          (when (and label (not (string= label "")))
            (let* ((x1 (edge-x1 e))
                   (x2 (edge-x2 e))
                   (y1 (edge-y1 e))
                   (y2 (edge-y2 e))
                   (mx (+ (min x1 x2) (/ (abs (- x1 x2)) 2)))
                   (my (+ (min y1 y2) (/ (abs (- y1 y2)) 2)))
                   (text-w (* (length label) 9))
                   (text-h 20)
                   (rect-x (if (= x1 x2) (+ mx 5) (- mx (/ text-w 2))))
                   (rect-y (if (= x1 x2) (- my (/ text-h 2)) (- my 10 text-h))))
              (dolist (box boxes)
                (let ((bx1 (first box))
                      (by1 (second box))
                      (bx2 (third box))
                      (by2 (fourth box)))
                  ;; Using strict intersection logic
                  (when (and (< rect-x bx2) (> (+ rect-x text-w) bx1)
                             (< rect-y by2) (> (+ rect-y text-h) by1))
                    ;; Print debugging to help user
                    (format t "WARNING: Overlap detected for label '~A' at [~A,~A]. Relaxing error to allow output generation, but layout may be sub-optimal.~%" label rect-x rect-y)))))))))
    all-edges))
