# LexicalAnalyzer-Haskell

## 1. Grammar:
G = (V, , P, E ) <br>
V = { E, A, 0, 1, x, y, +, -, *, (, ) }, <br>
 = { x, y, +, -, *, /, (, ) } <br>
P: – E 0 | 1 | x | y | (EAE) – A + | - | * | / <br>

## 2. Entry Details:
The input expression must be placed in the main function of the code and 
your modification must be done manually.

## 3. Output Details: 
If the input expression is a word generated by the grammar of
aritimetics expressions being used to call the main
return true, if not, return false.<br>
Examples:<br>
(0 + 1) = True <br>
(0 + 1x) + x = False
