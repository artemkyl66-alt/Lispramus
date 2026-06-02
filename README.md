# IDEF0 SVG Diagram Generation in Common Lisp

This project ports a Typst IDEF0 generation engine to Common Lisp, rendering SVG output with node, ICOM routing, and typography heuristics.

## Application Interface (HTML viewer)
Here is a screenshot of the requested UI displaying the results of the functional tests:
![Screenshot of the Interface](idef0-interface.png)

## Final Output Diagram
The third test generates a full decomposition. Here is the output:
![Final Diagram](final_diagram.svg)

## Test Coverage
Includes 10 positive unit tests and 10 negative unit tests enforcing error boundaries.
