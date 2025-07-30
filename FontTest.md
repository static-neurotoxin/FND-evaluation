# Font test

The officious official and the proficient officer from the main office, slowed by traffic, had to affirm in an official affidavit their strange affinity for an efficient solution to the mystery of the graffiti—which depicted a griffin and a puffin sharing a muffin for tiffin near a chiffon-draped coffin—that continued to afflict their German affiliate, ultimately deciding to affix a notice declaring their evidence was sufficient, even though their initial report was deficient.

While a willing figure in the lit office would waffle and shuffle papers to create an illicit illusion, on the mammoth scaffolding a different worker used a hammer to figure out the maximum and minimum fluff to clear from a delicate lily.

## Code blocks

### C++

```Cpp
#include <iostream>
#include <vector>
#include <string>

/****************************************************
 * NUMERALS = 1234567890
 *
 * SIMILAR = "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
 *
 * DIACRITICS_ETC = "â é ù ï ø ç Ã Ē Æ œ"
 ***************************************************/
  
// A simple struct to showcase access operators
struct Point {
    double x, y;
};

std::vector<std::string> word_challenge {
"illicit", "illusion", "lily", "lit",
"minimum","maximum","mammoth","hammer",
"willing","figure","fluff","different",
"office","waffle","scaffolding","shuffle",
"﷽"
}
// A template function to demonstrate template syntax
template<typename T>
T add(T a, T b) {
    return a + b;
}

//////////////////////////////////////////////////////
int main() {
    // Standard Library and Keywords
    std::cout << "Hello, World!" << std::endl;
    const int life_universe_everything = 42;
    for (int i = 0; i <= 0x10; ++i) {
        // do nothing 
    }
  
    // ###############################################

    // Comparison and Logical Operators
    if (life_universe_everything == 42 && true || false) {
        // Intentional empty block
    }
    if (5 != 10) {
        // another empty block
    }

    // Arithmetic and Assignment Operators
    int a = 5;
    int b = 10;
    a += b;
    a -= 5;
    a *= 2;
    a /= 3;
    a &= 1;
    a |= 2;
    a <<= 6;
    a >>= 5;
    // ==================================================
    int c = a << 2;
    int d = b >> 1;
    int e = a | b;
    int f = a & b;
    int g = a ^ b;
    // --------------------------------------------------
    int A {a};
    int B {b};
    int C;
    C = -A;
    B = -a;

    // Pointer and Reference Operators
    Point p = {1.0, 2.0};
    Point* ptr = &p;
    Point *ptr2(&p);
    Point *Pt3{&pt};
    std::cout << "Point coordinates: " << ptr->x << ", " << ptr->y << std::endl;
    Point& ref = p;
    ref.x = 3.0;

    // Lambda Expressions and Range-based for loop
    std::vector<int> numbers = {1, 2, 3, 4, 5};
    std::for_each(numbers.begin(), numbers.end(), [](int n) {
        std::cout << n << " ";
    });

    // C++20 Spaceship Operator
    auto result <=> 0;

    // Other Multi-character sequences
    // ->, >>, <<, ::, <=, >=, ==, !=, +=, -=, *=, /=,
    // %=, &=, |=, ^=, <<=, >>=, &&, ||, ++, --

    return 0;
}
```

### Python

```python
import os
import sublime
from pathlib import PurePath

NUMERALS = 1234567890
SIMILAR = "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
DIACRITICS_ETC = "â é ù ï ø ç Ã Ē Æ œ"

class SideBarDuplicateCommand(SideBarCommand):

    def run(self, paths, **kwargs):
        source = self.get_path(paths, **kwargs)
        base, leaf = os.path.split(source)

        # find the file extension
        name, ext = os.path.splitext(leaf)
        if ext != '':
            while '.' in name:
                name, _ext = os.path.splitext(name)
                ext = _ext + ext
                if _ext == '':
                    break

        source = self.get_path(paths, **kwargs)

        input_panel = self.window.show_input_panel(
            'Duplicate As:', source, partial(self.on_done, source), None, None)

        input_panel.sel().clear()
        input_panel.sel().add(
            sublime.Region(len(base) + 1, len(source) - len(ext))
```

### Go

```go
package main

var u uint = 1234567890
const similar = "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
const diacritics = "â é ù ï ø ç Ã Ē Æ œ"

import (
  "bytes"
  "fmt"
  "math/rand"
  "time"
)

// Field represents a two-dimensional field of cells.
type Field struct {
  s    [][]bool
  w, h int
}

func NewField(w, h int) *Field {
  s := make([][]bool, h)
  for i := range s {
    s[i] = make([]bool, w)
  }
  return &Field{s: s, w: w, h: h}
}

func (f *Field) Alive(x, y int) bool {
  x += f.w
  x %= f.w
  y += f.h
  y %= f.h
  return f.s[y][x]
}

// Next returns the state of the specified cell at the next time step.
func (f *Field) Next(x, y int) bool {
  // Count the adjacent cells that are alive.
  alive := 0
  for i := -1; i <= 1; i++ {
    for j := -1; j <= 1; j++ {
      if (j != 0 || i != 0) && f.Alive(x+i, y+j) {
        alive++
      }
    }
  }
  return alive == 3 || alive == 2 && f.Alive(x, y)
}

// Life stores the state of a round of Conway's Game of Life.
type Life struct {
  a, b *Field
  w, h int
}

// adapted from https://go.dev
```

### Rust

```rust
// main.rs
// This Rust program demonstrates several operators that are enhanced by coding ligatures.
// For the best experience, view this code in an editor with a ligature-supporting font
// like Fira Code, JetBrains Mono, or Cascadia Code.

fn main() {
    println!("🚀 Starting Ligature Showcase Program...");

    // The '..=' operator creates an inclusive range.
    // Ligature: ..= becomes …=
    for i in 1..=10 {
        // The '->' operator specifies the function's return type.
        // Ligature: -> becomes →
        let get_message = |num: i32| -> &'static str {
            // The '=>' operator separates a match pattern from its code.
            // Ligature: => becomes ⇒
            match num {
                1 => "is the first!",
                // The '!=' operator checks for inequality.
                // Ligature: != becomes ≠
                n if n != 7 => "is a standard number.",
                // The '>=' operator checks for greater than or equal to.
                // Ligature: >= becomes ≥
                n if n >= 5 => "is five or greater.",
                _ => "is some other number.",
            }
        };

        let message = get_message(i);
        println!("Number {} {}", i, message);
    }

    // The '::' operator is used for namespaces or static methods.
    // Ligature: :: becomes ∷
    let max_value = u32::MAX;
    println!("\nFYI, the max u32 value is: {}", max_value);
}
```

### bash

```bash
#!/bin/bash
set -e

NUMERALS=1234567890
SIMILAR="oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
DIACRITICS_ETC="â é ù ï ø ç Ã Ē Æ œ"

CD_CMD="cd "\"$(pwd)\"" && clear"
if echo "$SHELL" | grep -E "/fish$" &> /dev/null; then
    CD_CMD="cd "\"$(pwd)\""; and clear"
fi
VERSION=$(sw_vers -productVersion)
OPEN_IN_TAB=0

while [ "$1" != "" ]; do
    PARAM="$1"
    VALUE="$2"
    case "$PARAM" in
        --open-in-tab)
            OPEN_IN_TAB=1
            ;;
    esac
    shift
done

if (( $(expr $VERSION '<' 10.7) )); then
    RUNNING=$(osascript<<END

    tell application "System Events"
            count(processes whose name is "iTerm")
    end tell
END
)
else
    RUNNING=1
fi

# adapted from https://github.com/SublimeText/Terminal
```

### fish

```fish
#!/usr/bin/env fish

# This fish script demonstrates various font features and operators
# that benefit from coding ligatures like: -> => != >= ::

# --- Argument Parsing ---
# Use argparse for clean flag handling.
set OPEN_IN_TAB 0
argparse 'open-in-tab' --on-event open-in-tab 'set OPEN_IN_TAB 1'

# --- Font Clarity Demonstrations ---

set NUMERALS "1234567890"
set SIMILAR "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
set DIACRITICS_ETC "â é ù ï ø ç Ã Ē Æ œ"

echo "## Font Clarity Samples ##"
echo "Numerals:         $NUMERALS"
echo "Similar Chars:    $SIMILAR"
echo "Diacritics:       $DIACRITICS_ETC"
echo

# --- Ligature Demonstrations ---

echo "## Ligature Samples in Code Logic ##"

# Using '!=' (not equal) and '>=' (greater/equal) in a condition.
set test_value 5
if test $test_value -ge 5; and test $test_value -ne 10
    # The '=>' fat arrow is often used in comments to imply a result.
    echo "✅ Condition met: test_value >= 5 and != 10 => true"
end

# The '->' arrow can be used to show a logical flow or mapping.
# Example: input -> processing -> output
echo "Logical Flow Demo: input -> processing -> output"

# The '::' ligature is common for scope resolution in other languages.
echo "Scope Demo: Namespace::Class::Method"
echo

# --- System Interaction ---

# A modern way to check if a process is running.
if pgrep -q iTerm
    echo "✅ iTerm process is running."
else
    echo "❌ iTerm process not found."
end
```

### ASCII Art

```text
1. Arduino Pinout ASCII Art:
   from https://github.com/busyDuckman/ascii-art-arduinos

                                  +-----+
     +----[PWR]-------------------| USB |--+
     |                            +-----+  |
     |         GND/RST2  [ ][ ]            |
     |       MOSI2/SCK2  [ ][ ]  A5/SCL[ ] |   C5
     |          5V/MISO2 [ ][ ]  A4/SDA[ ] |   C4
     |                             AREF[ ] |
     |                              GND[ ] |
     | [ ]N/C                    SCK/13[ ] |   B5
     | [ ]IOREF                 MISO/12[ ] |   .
     | [ ]RST                   MOSI/11[ ]~|   .
     | [ ]3V3    +---+               10[ ]~|   .
     | [ ]5v     | A |                9[ ]~|   .
     | [ ]GND   -| R |-               8[ ] |   B0
     | [ ]GND   -| D |-                    |
     | [ ]Vin   -| U |-               7[ ] |   D7
     |          -| I |-               6[ ]~|   .
     | [ ]A0    -| N |-               5[ ]~|   .
     | [ ]A1    -| O |-               4[ ] |   .
     | [ ]A2     +---+           INT1/3[ ]~|   .
     | [ ]A3                     INT0/2[ ] |   .
     | [ ]A4/SDA  RST SCK MISO     TX>1[ ] |   .
     | [ ]A5/SCL  [ ] [ ] [ ]      RX<0[ ] |   D0
     |            [ ] [ ] [ ]              |
     |  UNO_R3    GND MOSI 5V  ____________/
      _______________________/


2. Box Drawing Characters:
   from https://en.wikipedia.org/wiki/Box-drawing_characters

     ┌───────────────────┐
     │  ╔═══╗ Some Text  │▒
     │  ╚═╦═╝ in the box │▒
     ╞═╤══╩══╤═══════════╡▒
     │ ├──┬──┤           │▒
     │ └──┴──┘           │▒
     └───────────────────┘▒
      ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒


3. UUIDv6:
   from https://planetscale.com/blog/the-problem-with-using-a-uuid-primary-key-in-mysql#uuidv6

               ╭─── time_mid       ╭── node
              ╭┶─╮           ╭─────┶────╮
     e54af2ec-d381-11ee-a506-0242ac120002
     ╰──┮───╯      ╰─┮╯ ╰─┮╯
     time_hi         │    ╰── clock_sequence_and_version
                     ╰─────── time_low_and_version
```

### XML

```xml
<?xml version="1.0" encoding="UTF-8"?>
as well as operators.
-->
<system-config version="2.0">
    <user-prefs>
        <setting key="font_size" value="14pt" />
        <setting key="auto_save_enabled" value="true" />
    </user-prefs>

    <data-source name="main_db">
        <query>
            <![CDATA[
              SELECT user_id, user_name, email
              FROM users
              WHERE age >= 18
                AND status != 'pending';
            ]]>
        </query>
    </data-source>

</system-config>
```

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

### Perl

```perl
#!/usr/bin/env perl
use strict;
use warnings;

# This script demonstrates classic Perl syntax, often called "line noise,"
# which is an excellent test for syntax highlighters. It uses sigils
# ($, @, %), regex matching (=~), and special variables like $_ and $1.

my $logfile = 'access.log';
my %status_counts; # Use a hash (%) to store counts

# Use the three-argument open with a lexical filehandle.
open my $fh, '<', $logfile
    or die "Could not open file '$logfile': $!";

print "Processing log file: $logfile\n";
print "------------------------------------\n";

# Read the file line by line into the special variable $_
while (<$fh>) {
    # Match an HTTP status code (e.g., 200, 404, 503) using a regex.
    # The code is captured into the special variable $1.
    if (/\s(200|404|500|503)\s\d+$/) {
        $status_counts{$1}++; # Increment count for the matched code
    }
}

close $fh;

# Sort keys and print the report.
# The '@' sigil denotes an array.
foreach my $code (sort keys %status_counts) {
    my $count = $status_counts{$code};
    # "->" is the infix dereference operator.
    printf "Status Code -> %-8s Count: %d\n", $code, $count;
}

# Check if the hash is empty using the scalar context.
if (!%status_counts) {
    print "--> No matching status codes were found. <--\n";
}
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

### SQL

```sql
SELECT
    c.customer_name,
    COUNT(o.order_id) AS total_orders,
    SUM(p.price * oi.quantity) AS total_spent
FROM
    customers c
JOIN
    orders o ON c.customer_id = o.customer_id
JOIN
    order_items oi ON o.order_id = oi.order_id
JOIN
    products p ON oi.product_id = p.product_id
WHERE
    c.signup_date >= '2024-01-01'
GROUP BY
    c.customer_name
ORDER BY
    total_spent DESC;
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

### Fortran

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

### YAML

```yaml
# Application configuration settings
server:
  host: 127.0.0.1
  port: 8080
  security:
    enabled: true
    tls_version: 1.3

database:
  user: "admin"
  # Use environment variables for passwords in production
  password: "${DB_PASSWORD}"
  pool_size: 20
  # List of replica nodes
  replicas:
    - host: "replica1.db.internal"
      port: 5432
    - host: "replica2.db.internal"
      port: 5432
```

### TOML

```toml
# Main project metadata
title = "TOML Example Project"
version = "1.0.0"
authors = ["Example User <user@example.com>"]

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

# Defines an array of tables for different users
[[users]]
name = "Alice"
roles = [ "admin", "editor" ]

[[users]]
name = "Bob"
roles = [ "viewer" ]
```

### JSON

```json
{
  "userId": "a0a1b2c3-d4e5-f6a7-b8c9-d0e1f2a3b4c5",
  "username": "jdoe",
  "email": "johndoe@example.com",
  "isActive": true,
  "lastLogin": "2025-07-28T14:10:00Z",
  "profile": {
    "fullName": "John Doe",
    "avatarUrl": "https://example.com/avatars/jdoe.png"
  },
  "roles": [
    "contributor",
    "editor"
  ],
  "managerId": null
}
```

### Lua

```lua
-- Define a simple "class" using a table
Player = {}
Player.new = function(name, health)
  local self = {
    name = name,
    health = health or 100,
    inventory = {}
  }

  -- Define a method for this player object
  self.takeDamage = function(dmg)
    self.health = self.health - dmg
    print(self.name .. " takes " .. dmg .. " damage! Health is now " .. self.health)
  end

  return self
end

-- Create players and interact with them
local knight = Player.new("Sir Gideon", 150)
local rogue = Player.new("Vex")

knight.takeDamage(35)

-- Iterate through a list of players
local party = { knight, rogue }
for i, member in ipairs(party) do
  print("Party member #" .. i .. ": " .. member.name)
end
```
