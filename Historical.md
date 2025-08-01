# Historic computer languages

Generally, these are languages that were once widely used, but have fallen out of favor. This is totally an arbitrary decision on my part, there is no objective metric why I have included a language here.

## Code blocks

### APL

```apl
⍝ Conway's Game of Life - A Font & Syntax Torture Test
⍝ This simulation uses a wide array of APL glyphs, operators, and
⍝ control structures. It's designed to challenge font rendering
⍝ (⍴ ⌽ ⍉ ∊ ← →), syntax highlighting, and Unicode support.

∇ LifeSimulate boardSize; board; step; Neighbours; IsAlive
  ⍝ Define a function 'IsAlive' to calculate the next generation.
  IsAlive ← {
      ⍝ ⍵ ← current board (binary matrix).
      ⍝ The core of APL's power: a "one-liner" to find all neighbours.
      ⍝ It uses an outer product (∘.) with rotation (⌽) applied to each (¨)
      ⍝ element in the matrix to generate the sum of 8 neighbours.
      Neighbours ← +/,(¯1 0 1∘.⊖¯1 0 1)⌽¨⊂⍵
      
      ⍝ Apply the rules of life using a concise boolean expression.
      ⍝ A cell becomes/stays alive if it has 3 neighbours, OR if it's
      ⍝ already alive (⍵) and has 2 neighbours. Note ∨(OR), ∧(AND).
      (Neighbours=3)∨(⍵∧Neighbours=2)
  }

  ⍝ Initialize the board with a random state (~25% alive).
  ⍝ Note ← (assignment), ? (random roll), and ⍴ (reshape).
  board ← 1=?boardSize⍴4

  :For step :In ⍳50 ⍝ Loop 50 times (⍳ creates a vector 1 to 50).
      ⎕←⎕CLS ⍝ Clear the screen (a system command).
      ⎕←(⎕A,' ■')[board] ⍝ Display board: 0→' ', 1→'■'.
      ⎕DL 0.1 ⍝ Delay for 0.1 seconds.
      board ← IsAlive board ⍝ Calculate the next state.
  :EndFor
  '∇ Simulation Complete ∇'
∇
```

### 6x09 Assembly

```asm
******************************************************************************
*
* Sieve of Eratosthenes for Motorola 6809/Hitachi 6309
*
* Finds all prime numbers up to MAX_NUM. This implementation uses a
* bitmap optimized to store only odd numbers, which is a common
* and efficient technique for memory-constrained systems.
*
******************************************************************************

          ORG    $1000        ; Code origin

*========= Constants and Memory Reservation ==================================

MAX_NUM   EQU    8191         ; Find primes up to this number (must be odd)
SQRT_MAX  EQU    90           ; Sqrt(MAX_NUM), loop limit for outer loop
BUF_SIZE  EQU    (MAX_NUM-1)/2/8 ; Bitmap size in bytes: 511 bytes

RAM_START EQU    $2000        ; Start of available RAM
FLAGS     EQU    RAM_START    ; Start of our prime/composite bitmap

*========= Main Sieve Routine ================================================

SIEVE
          PSHS   U,Y          ; Save registers that will be modified

* --- Step 1: Initialize the entire bitmap to 0 (all numbers prime) ---
* On a Hitachi 6309, this could be made faster with the LDMD
* instruction for block memory transfers.

          LDX    #FLAGS       ; X points to the start of the bitmap
          LDD    #0           ; Load D with zeros
          LDU    #BUF_SIZE/2  ; Loop U times (BUF_SIZE must be even)
CLR_LOOP
          STD    ,X++         ; Clear two bytes at a time
          LEAU   -1,U         ; Decrement loop counter
          BNE    CLR_LOOP     ; Continue until buffer is clear

* --- Step 2: Main loop to find the next prime 'p' ---
* The outer loop finds the next prime 'p' to use for sieving.
* The number p is represented by its index 'p_idx' in the bitmap.
* p = (p_idx * 2) + 3. We iterate p_idx from 0 up to a limit
* derived from SQRT_MAX.
*
* Y = p_idx (index for the current prime)

          LDY    #0           ; Start with p_idx = 0 (which is prime 3)
OUTER_LOOP
          JSR    IS_BIT_SET   ; Check if FLAGS[p_idx] is already marked
          BNE    NEXT_PRIME   ; If bit is set (1), this number is composite...
                              ; ...so we skip to the next potential prime.

* --- Found a prime! Now mark all of its multiples. ---
* p = (Y * 2) + 3
* First multiple to mark is p*p.
* The index for p*p is k_idx = (p*p - 3) / 2
*
* D = prime 'p'
* X = index 'k_idx' for marking multiples

          LDD    Y,Y          ; D = Y*2 (p_idx * 2)
          ADDD   #3           ; D = Y*2 + 3 (this is our prime 'p')
          PSHS   D            ; Save 'p' on the stack

          MUL                 ; A = high(p*p), B = low(p*p) -> D = p*p
          SUBD   #3           ; D = p*p -
```

### Basic

```basic
10 REM GUESS THE NUMBER GAME
20 PRINT "I'M THINKING OF A NUMBER BETWEEN 1 AND 100."
30 LET N = INT(RND(1) * 100) + 1
40 PRINT "ENTER YOUR GUESS:";
50 INPUT G
60 IF G < N THEN PRINT "TOO LOW! TRY AGAIN." : GOTO 40
70 IF G > N THEN PRINT "TOO HIGH! TRY AGAIN." : GOTO 40
80 PRINT "YOU GUESSED IT! THE NUMBER WAS "; N
```

### ALGOL

```algol
begin
    integer procedure Factorial(n);
    value n; integer n;
    begin
        Factorial := if n = 0 then 1 else n * Factorial(n - 1);
    end;

    for i := 1 step 1 until 10 do
        comment This loop calculates and prints factorials;
        outinteger(1, i);
        outstring(1, "! = ");
        outinteger(1, Factorial(i));
        newline(1);
end
```

### Prolog

```prolog
% Facts: parent(Parent, Child)
parent(john, mary).
parent(john, david).
parent(susan, mary).
parent(susan, david).
parent(david, emily).

% Rule: grandparent(Grandparent, Grandchild)
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).

% Query to find Emily's grandparent:
% ?- grandparent(X, emily).
% Expected result: X = john ; X = susan.
```

### Lisp

```lisp
;;; A recursive factorial function in Common Lisp
(defun factorial (n)
  "Calculates the factorial of a non-negative integer."
  (if (<= n 1)
      1 ; Base case: factorial of 0 or 1 is 1
      (* n (factorial (- n 1))))) ; Recursive step

;;; Example usage
(format t "The factorial of 5 is ~d~%" (factorial 5))
```

### Fortran 77

```fortran
C     ==================================================================
C     PROGRAM TO CALCULATE THE AREA OF A CIRCLE
C     This demonstrates basic I/O and arithmetic in FORTRAN 77.
C     ==================================================================
      PROGRAM CIRCLE
      REAL RADIUS, AREA
      PARAMETER (PI = 3.14159)

      PRINT *, 'ENTER THE RADIUS OF THE CIRCLE:'
      READ *, RADIUS

      IF (RADIUS .LT. 0.0) THEN
          PRINT *, 'RADIUS CANNOT BE NEGATIVE.'
      ELSE
          AREA = PI * RADIUS**2
          PRINT 100, RADIUS, AREA
      END IF

 100  FORMAT('FOR A CIRCLE WITH RADIUS ', F10.4, ', THE AREA IS ', F12.4)
      STOP
      END
```

### Smalltalk

```smalltalk
"Calculate the sum of the squares from 1 to 100"
| sum |
sum := 0.

1 to: 100 do: [ :i |
    sum := sum + (i * i).
].

Transcript show: 'Sum of squares is: ', sum printString; cr.
```
