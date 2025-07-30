# TIOBE top 20 programming languages

From July 2025

## Python

From (programmingfonts.org)

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

## C++

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

## C

Inspired by K&R C

```c
#include<stdio.h>

int main(int *argc, char *argv[])
{
    printf("Hello World\n");

    return 0;
}
```

## Java

From [Wikipedia](https://en.wikipedia.org/wiki/Java_(programming_language))

```java
public class Example {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}
```

## C#

From [Wikipedia](https://en.wikipedia.org/wiki/C_Sharp_(programming_language))

```csharp
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Hello, world!");
    }
}
```

## JavaScript

From (programmingfonts.org)

```js
import { Cookies } from './cookies.js'
import { Samples } from './samples.js'

const numerals = 1234567890
const similar = "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_="
const diacritics_etc = "â é ù ï ø ç Ã Ē Æ œ"

export class Language {
  el = document.getElementById('select-language')
  samples = new Samples

  // set initial value and start listening
  init () {
    if (Cookies.get('language')) {
      this.el.value = Cookies.get('language')
    }
    this.el.onchange = () => {
      this.set()
    }
    this.set()
  }

  set () {
    const lang = this.el.value

    window.CMeditor.doc.setValue(this.samples.get(lang))
    window.CMeditor.setOption('mode', lang.toLowerCase())
    window.CMeditor.refresh()

    Cookies.set('language', lang)
  }
}
```

## Go

From (programmingfonts.org)

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

## Visual Basic

From [Wikipedia](https://en.wikipedia.org/wiki/Visual_Basic_(.NET))

```vbnet
Module Module1

    Sub Main()
        ' The classic "Hello, World!" demonstration program
        Console.WriteLine("Hello, World!")
    End Sub

End Module
```

## Ada

From [Wikipedia](https://en.wikipedia.org/wiki/Ada_(programming_language))

```ada
with Ada.Text_IO;
procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello, world!");
end Hello;
```

## Delphi/Object Pascal

From [Wikipedia](https://en.wikipedia.org/wiki/Delphi_(software))

```delphi
program ObjectPascalExample;

type
  THelloWorld = class
    procedure Put;
  end;

procedure THelloWorld.Put;
begin
  Writeln('Hello, World!');
end;

var
  HelloWorld: THelloWorld;               { this is an implicit pointer }

begin
  HelloWorld := THelloWorld.Create;      { constructor returns a pointer to an object of type THelloWorld }
  HelloWorld.Put;
  HelloWorld.Free;                       { this line deallocates the THelloWorld object pointed to by HelloWorld }
end.
```

## Perl

From [Wikipedia](https://en.wikipedia.org/wiki/Perl)

```perl
#!/usr/bin/env perl
use strict;
use warnings;

my ( $remaining, $total );

$remaining=$total=shift(@ARGV);

STDOUT->autoflush(1);

while ( $remaining ) {
    printf ( "Remaining %s/%s \r", $remaining--, $total );
    sleep 1;
}

print "\n";
```

## Fortran

Fortran 77 sample from [Wikipedia](https://en.wikipedia.org/wiki/Fortran)

```fortran
      PROGRAM HERON
C AREA OF A TRIANGLE WITH A STANDARD SQUARE ROOT FUNCTION
C INPUT - DEFAULT STANDARD INPUT UNIT, INTEGER INPUT
C OUTPUT - DEFAULT STANDARD OUTPUT UNIT, REAL OUTPUT
C INPUT ERROR DISPLAY ERROR OUTPUT CODE 1 IN JOB CONTROL LISTING
      READ (*, *) IA, IB, IC
C
C IA, IB, AND IC MAY NOT BE NEGATIVE OR ZERO
C FURTHERMORE, THE SUM OF TWO SIDES OF A TRIANGLE
C MUST BE GREATER THAN THE THIRD SIDE, SO WE CHECK FOR THAT, TOO
      IF (IA .LE. 0 .OR. IB .LE. 0 .OR. IC .LE. 0) THEN
        WRITE (*, *) 'IA, IB, and IC must be greater than zero.'
        STOP 1
      END IF
C
      IF (IA+IB-IC .LE. 0
     +    .OR. IA+IC-IB .LE. 0
     +    .OR. IB+IC-IA .LE. 0) THEN
        WRITE (*, *) 'Sum of two sides must be greater than third side.'
        STOP 1
      END IF
C
C USING HERON'S FORMULA WE CALCULATE THE
C AREA OF THE TRIANGLE
      S = (IA + IB + IC) / 2.0
      AREA = SQRT ( S * (S - IA) * (S - IB) * (S - IC))
      WRITE (*, 601) IA, IB, IC, AREA
  601 FORMAT ('A= ', I5, '  B= ', I5, '  C= ', I5, '  AREA= ', F10.2,
     +        ' square units')
      STOP
      END
```

## SQL

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

## PHP

From (programmingfonts.org)

```php
<?php

namespace AppController;

use SymfonyComponentHttpFoundationResponse;
use SymfonyComponentRoutingAttributeRoute;

class LuckyController
{
  const NUMERALS = 1234567890;
  const SIMILAR = "oO08 iIlL1 g9qCGQ 8%& <([{}])> .,;: -_=";
  const DIACRITICS_ETC = "â é ù ï ø ç Ã Ē Æ œ";

    #[Route('/lucky/number/{max}', name: 'app_lucky_number')]
    public function number(
    int $max,

        #[Autowire(service: 'monolog.logger.request')]
        LoggerInterface $logger
    ): Response
    {
        $logger->info('We are logging!');
        $number = random_int(0, $max);

        return new Response(
            '<html><body>Lucky number: '.$number.'</body></html>'
        );
    }
}

// adapted from https://symfony.com/doc
>
```

## R

From [Wikipedia](https://en.wikipedia.org/wiki/R_(programming_language))

```r
# The function's input parameters are x and y.
# The function, named f, returns a linear combination of x and y.
f <- function(x, y) {
  z <- 3 * x + 4 * y

  # An explicit return() statement is optional--it could be replaced with simply `z` in this case.
  return(z)
}
```

## MATLAB

From [Wikipedia](https://en.wikipedia.org/wiki/MATLAB)

```matlab
rgbImage = imread('ecg.png');
grayImage = rgb2gray(rgbImage); % for non-indexed images
level = graythresh(grayImage); % threshold for converting image to binary, 
binaryImage = im2bw(grayImage, level); 
% Extract the individual red, green, and blue color channels.
redChannel = rgbImage(:, :, 1);
greenChannel = rgbImage(:, :, 2);
blueChannel = rgbImage(:, :, 3);
% Make the black parts pure red.
redChannel(~binaryImage) = 255;
greenChannel(~binaryImage) = 0;
blueChannel(~binaryImage) = 0;
% Now recombine to form the output image.
rgbImageOut = cat(3, redChannel, greenChannel, blueChannel);
imshow(rgbImageOut);
```


## Rust

From [Wikipedia](https://en.wikipedia.org/wiki/Rust_(programming_language))

```rust
fn main() {
    let x = 10;
    if x > 5 {
        println!("value is greater than five");
    }

    if x % 7 == 0 {
        println!("value is divisible by 7");
    } else if x % 5 == 0 {
        println!("value is divisible by 5");
    } else {
        println!("value is not divisible by 7 or 5");
    }
}
```

## Assembly Language

From [Wikipedia](https://en.wikipedia.org/wiki/Assembly_language)

```nasm
section	.text
   global _start
	
_start:	        
   mov	edx,len     ; length of string, third argument to write()
   mov	ecx,msg     ; address of string, second argument to write()
   mov	ebx,1       ; file descriptor (standard output), first argument to write()
   mov	eax,4       ; system call number for write()
   int	0x80        ; system call trap
	
   mov	ebx,0       ; exit code, first argument to exit()
   mov	eax,1       ; system call number for exit()
   int	0x80        ; system call trap

section	.data
msg db 'Hello, world!', 0xa  
len equ $ - msg
```

## Kotlin

From [Wikipedia](https://en.wikipedia.org/wiki/Kotlin_(programming_language))

```kotlin
// Hello, World! example
fun main() {
    val scope = "World"
    println("Hello, $scope!")
}
```
