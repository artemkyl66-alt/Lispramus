# IDEF0 SVG Diagram Generation in Common Lisp

This project ports a Typst IDEF0 generation engine to Common Lisp, rendering SVG output with node, ICOM routing, typography heuristics, and styling.

## Functional Tests & Outputs

As requested, below are the screenshots demonstrating the three functional tests, alongside their resulting SVG diagram screenshots.

### Pair 1: Styled Node Diagram
**Interface View (Typst Definition vs Result)**
![Interface 1](interface1.png)

**Diagram Result (SVG View)**
![Result 1](result1.png)


### Pair 2: Default Color Node Diagram
**Interface View (Typst Definition vs Result)**
![Interface 2](interface2.png)

**Diagram Result (SVG View)**
![Result 2](result2.png)


### Pair 3: Decomposition Network Diagram
**Interface View (Typst Definition vs Result)**
![Interface 3](interface3.png)

**Diagram Result (SVG View)**
![Result 3](result3.png)


## Test Coverage
Includes 10 positive unit tests and 10 negative unit tests enforcing error boundaries and checking edge connection logic.
