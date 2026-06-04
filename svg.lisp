(in-package :idef0.svg)

;; Error conditions
(define-condition svg-error (error) ())
(define-condition invalid-edge-coordinates-error (svg-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Edge missing one or more coordinates (x1, y1, x2, y2)"))))
(define-condition missing-icom-arrays-error (svg-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Missing mandatory data for SVG rendering (e.g. nil passed directly)"))))


(defun edge-coordinates-valid-p (edge)
  (and (edge-x1 edge) (edge-y1 edge) (edge-x2 edge) (edge-y2 edge)))

(defun render-node (node)
  (let ((x (node-x node))
        (y (node-y node))
        (width (node-width node))
        (height (node-height node))
        (title (node-title node))
        (number (node-number node))
        (cost (node-cost node)))
    (with-output-to-string (s)
      (format s "  <g class=\"node\" transform=\"translate(~A, ~A)\">~%" x y)
      (format s "    <rect width=\"~A\" height=\"~A\" fill=\"white\" stroke=\"black\" stroke-width=\"2\"/>~%" width height)
      (format s "    <text x=\"~A\" y=\"~A\" text-anchor=\"middle\" dominant-baseline=\"middle\" font-size=\"16\">~A</text>~%"
              (/ width 2) (/ height 2) title)
      (when number
        (format s "    <text x=\"~A\" y=\"~A\" text-anchor=\"end\" dominant-baseline=\"auto\" font-size=\"14\">~A</text>~%"
                (- width 5) (- height 5) number))
      (when cost
        (format s "    <text x=\"5\" y=\"~A\" text-anchor=\"start\" dominant-baseline=\"auto\" font-size=\"14\">~A</text>~%"
                (- height 5) cost))
      (format s "  </g>~%"))))

(defun get-hex-color-id (color)
  (remove #\# color))

(defun render-edge (edge)
  (unless (edge-coordinates-valid-p edge)
    (error 'invalid-edge-coordinates-error))
  (let* ((x1 (edge-x1 edge))
         (y1 (edge-y1 edge))
         (x2 (edge-x2 edge))
         (y2 (edge-y2 edge))
         (label (edge-label edge))
         (color (edge-color edge))
         (rev (edge-rev edge))
         (color-id (get-hex-color-id color))
         (marker (if rev (format nil "url(#arrow-start-~A)" color-id) (format nil "url(#arrow-end-~A)" color-id))))
    (with-output-to-string (s)
      (format s "  <g class=\"edge\">~%")
      (format s "    <path d=\"M ~A ~A L ~A ~A\" stroke=\"~A\" stroke-width=\"1\" marker-~A=\"~A\" fill=\"none\"/>~%"
              x1 y1 x2 y2 color
              (if rev "start" "end") marker)
      (when (and label (not (string= label "")))
        ;; heuristic midpoint for label
        (let* ((mx (+ (min x1 x2) (/ (abs (- x1 x2)) 2)))
               (my (+ (min y1 y2) (/ (abs (- y1 y2)) 2)))
               (text-w (* (length label) 9))
               (text-h 20)
               (rect-x 0) (rect-y 0) (text-x 0) (text-y 0))
          ;; adjust if vertical vs horizontal
          (if (= x1 x2)
              (progn
                (setf rect-x (+ mx 5)
                      rect-y (- my (/ text-h 2))
                      text-x (+ mx 5)
                      text-y my)
                (format s "    <rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" fill=\"white\"/>~%" rect-x rect-y text-w text-h)
                (format s "    <text x=\"~A\" y=\"~A\" dominant-baseline=\"middle\" font-size=\"16\" fill=\"~A\">~A</text>~%" text-x text-y color label))
              (progn
                (setf rect-x (- mx (/ text-w 2))
                      rect-y (- my 10 text-h)
                      text-x mx
                      text-y (- my 15))
                (format s "    <rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" fill=\"white\"/>~%" rect-x rect-y text-w text-h)
                (format s "    <text x=\"~A\" y=\"~A\" text-anchor=\"middle\" dominant-baseline=\"middle\" font-size=\"16\" fill=\"~A\">~A</text>~%" text-x text-y color label)))))
      (format s "  </g>~%"))))

(defun get-unique-colors (edges)
  (let ((colors nil))
    (dolist (e edges)
      (pushnew (edge-color e) colors :test #'string=))
    colors))

(defun render-svg (nodes connections)
  ;; For test-empty-icom-arrays we might be passed nil intentionally or somehow missing data.
  ;; We must accept nil for empty nodes/connections, but if someone calls with malformed nodes list containing nils,
  ;; we should error. Let's make sure 'nodes' is a list and not just something that breaks.
  (unless (listp nodes)
    (error 'missing-icom-arrays-error))

  (let* ((edges (calculate-layout nodes connections))
         (min-x 0) (min-y 0) (max-x 0) (max-y 0))

    (dolist (n nodes)
      (setf min-x (min min-x (node-x n)))
      (setf min-y (min min-y (node-y n)))
      (setf max-x (max max-x (+ (node-x n) (node-width n))))
      (setf max-y (max max-y (+ (node-y n) (node-height n)))))

    (dolist (e edges)
      (when (edge-coordinates-valid-p e)
        (setf min-x (min min-x (edge-x1 e) (edge-x2 e)))
        (setf min-y (min min-y (edge-y1 e) (edge-y2 e)))
        (setf max-x (max max-x (edge-x1 e) (edge-x2 e)))
        (setf max-y (max max-y (edge-y1 e) (edge-y2 e)))))

    ;; Add some padding
    (setf min-x (- min-x 20))
    (setf min-y (- min-y 20))
    (setf max-x (+ max-x 20))
    (setf max-y (+ max-y 20))

    (let ((w (max 1 (- max-x min-x)))
          (h (max 1 (- max-y min-y)))
          (colors (get-unique-colors edges)))
    (with-output-to-string (s)
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"~A ~A ~A ~A\" width=\"100%\" height=\"100%\">~%" min-x min-y w h)
      (format s "  <defs>~%")
      (dolist (color colors)
        (let ((cid (get-hex-color-id color)))
          (format s "    <marker id=\"arrow-end-~A\" markerWidth=\"10\" markerHeight=\"10\" refX=\"9\" refY=\"3\" orient=\"auto\">~%" cid)
          (format s "      <path d=\"M0,0 L0,6 L9,3 z\" fill=\"~A\" />~%" color)
          (format s "    </marker>~%")
          (format s "    <marker id=\"arrow-start-~A\" markerWidth=\"10\" markerHeight=\"10\" refX=\"1\" refY=\"3\" orient=\"auto\">~%" cid)
          (format s "      <path d=\"M9,0 L9,6 L0,3 z\" fill=\"~A\" />~%" color)
          (format s "    </marker>~%")))
      (format s "  </defs>~%")
      (dolist (n nodes)
        (format s "~A" (render-node n)))
      (dolist (e edges)
        (format s "~A" (render-edge e)))
      (format s "</svg>~%")))))
