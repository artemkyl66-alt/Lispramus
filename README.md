# IDEF0 SVG Diagram Generation in Common Lisp

This project ports an IDEF0 generation engine to Common Lisp, rendering SVG output with node, ICOM routing, typography heuristics, and styling directly from Lisp datastructures. It supports an advanced orthogonal collision avoidance routing algorithm for rendering decomposition loops and complex feedback scenarios cleanly.

## Functional Tests & Outputs

As requested, below are the screenshots demonstrating the three functional tests using Lisp data syntax (no Typst), alongside their resulting SVG diagram screenshots with improved text sizing and layout boundaries.

### Pair 1: Styled Node Diagram
**Interface View (Lisp Data Definition vs Result)**
![Interface 1](interface1.png)

**Diagram Result (SVG View)**
![Result 1](result1.png)


### Pair 2: Default Color Node Diagram
**Interface View (Lisp Data Definition vs Result)**
![Interface 2](interface2.png)

**Diagram Result (SVG View)**
![Result 2](result2.png)


### Pair 3: Complex Decomposition Network Diagram
Features multiple input/outputs and feedback routing without colliding logic lines.
**Interface View (Lisp Data Definition vs Result)**
![Interface 3](interface3.png)

**Diagram Result (SVG View)**
![Result 3](result3.png)


## Test Coverage
Includes 10 positive unit tests and 10 negative unit tests enforcing error boundaries and checking edge connection logic.
