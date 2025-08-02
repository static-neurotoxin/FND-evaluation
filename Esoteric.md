# Esoteric languages

This is a collection of languages that are esoteric, weird or just plain wrong. Some are well known, others not so much. I am using [Esolang](https://esolangs.org/wiki/Main_Page) for code samples, licensed under [CC0 public domain dedication](https://esolangs.org/wiki/Esolang:Copyrights).

## INTERCAL

```intercal
assignment statement (called CALCULATE, but that keyword is never used)
   DO variable <- expression
STASH (push variables on their own stacks)
   DO STASH variable + variable + ... + variable
RETRIEVE (pop variables from their own stacks)
   DO RETRIEVE variable + variable + ... + variable
NEXT (jump to command, save return address on an 80-address stack)
   DO (label) NEXT
RESUME (pop <expression> return addresses, jump to the last one popped)
   DO RESUME expression
FORGET (pop <expression> return addresses and discard them)
   DO FORGET expression
ABSTAIN (don't execute the referenced line / lines)
   DO ABSTAIN FROM (label)
or DO ABSTAIN FROM something + something + ... + something
(as in DO ABSTAIN FROM CALCULATING)
REINSTATE (cancel out an ABSTAIN or DON'T)
   DO REINSTATE (label)
or DO REINSTATE something + something + ... + something
IGNORE (make a variable read-only, silently ignoring writes)
   DO IGNORE variable + variable + ... + variable
REMEMBER (cancel out an IGNORE)
   DO REMEMBER variable + variable + ... + variable
WRITE IN (input, using digits spelled out as words)
   DO WRITE IN variable
READ OUT (output, in butchered Roman numerals)
   DO READ OUT variable
```

## Brainfuck

```brainfuck
 1 +++++ +++               Set Cell #0 to 8
 2 [
 3     >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
 4     [                   as the cell will be cleared by the loop
 5         >++             Add 4*2 to Cell #2
 6         >+++            Add 4*3 to Cell #3
 7         >+++            Add 4*3 to Cell #4
 8         >+              Add 4 to Cell #5
 9         <<<<-           Decrement the loop counter in Cell #1
10     ]                   Loop till Cell #1 is zero
11     >+                  Add 1 to Cell #2
12     >+                  Add 1 to Cell #3
13     >-                  Subtract 1 from Cell #4
14     >>+                 Add 1 to Cell #6
15     [<]                 Move back to the first zero cell you find; this will
16                         be Cell #1 which was cleared by the previous loop
17     <-                  Decrement the loop Counter in Cell #0
18 ]                       Loop till Cell #0 is zero
19 
20 The result of this is:
21 Cell No :   0   1   2   3   4   5   6
22 Contents:   0   0  72 104  88  32   8
23 Pointer :   ^
24 
25 >>.                     Cell #2 has value 72 which is 'H'
26 >---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
27 +++++ ++..+++.          Likewise for 'llo' from Cell #3
28 >>.                     Cell #5 is 32 for the space
29 <-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
30 <.                      Cell #3 was set to 'o' from the end of 'Hello'
31 +++.----- -.----- ---.  Cell #3 for 'rl' and 'd'
32 >>+.                    Add 1 to Cell #5 gives us an exclamation point
33 >++.                    And finally a newline from Cell #6
```

## LOLCODE

```lolcode
HAI 1.3
O HAI IM pile
	I HAS A length ITZ 0
	I HAS A max ITZ -1
	
	HOW IZ I pushin YR item
		DIFFRINT ME'Z max AN BIGGR OF ME'Z max AN ME'Z length, O RLY?
			YA RLY, ME HAS A SRS ME'Z length ITZ item, ME'Z max R SUM OF ME'Z max AN 1
			NO WAI, ME'Z SRS ME'Z length R item
		OIC
		ME'Z length R SUM OF ME'Z length AN 1
	IF U SAY SO
	
	HOW IZ I popin
		DIFFRINT ME'Z length AN 0, O RLY?
		YA RLY
			ME'Z length R DIFF OF ME'Z length AN 1
			I HAS A item ITZ ME'Z SRS ME'Z length
			ME'Z SRS ME'Z length R NOOB
			FOUND YR item
		OIC
	IF U SAY SO
	
	HOW IZ I gettinLen
		FOUND YR ME'Z length
	IF U SAY SO
KTHX

I HAS A stack ITZ LIEK A pile

stack IZ pushin YR "testvalue" MKAY
stack IZ pushin YR "value2" MKAY
VISIBLE stack IZ popin MKAY
stack IZ pushin YR "lolcat" MKAY
stack IZ pushin YR "longcat" MKAY
VISIBLE stack IZ popin MKAY
VISIBLE stack IZ popin MKAY
VISIBLE stack IZ popin MKAY

KTHXBYE
```

## Ook

```ook
Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook? Ook! Ook! Ook? Ook! Ook? Ook.
Ook! Ook. Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook?
Ook! Ook! Ook? Ook! Ook? Ook. Ook. Ook. Ook! Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook! Ook. Ook! Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook! Ook. Ook. Ook? Ook. Ook? Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook? Ook! Ook! Ook? Ook! Ook? Ook. Ook! Ook.
Ook. Ook? Ook. Ook? Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook? Ook! Ook! Ook? Ook! Ook? Ook. Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook.
Ook? Ook. Ook? Ook. Ook? Ook. Ook? Ook. Ook! Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook! Ook. Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook.
Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook!
Ook! Ook. Ook. Ook? Ook. Ook? Ook. Ook. Ook! Ook.
```

## PHL 1.0

```phl
main[
    set(A, 0);
    set(B, 1);
    set(C, 0);
    set(I, 0);
    loop[
        if(get(I)<10, [
            set(C, add(get(A), get(B)));
            set(A, get(B));
            set(B, get(C));
            set(I, add(get(I), 1));
            println(get(I): get(C));
            loop();
        ], []);
    ]();
]
```

## Qwerty Reverse Polish Notation

```qrpn
 @ =value               // value = input_number();
 1 =result              // result = 1;
 $value =i              // i = value;
 >loop                  // loop:
  $result $i * =result  //   result *= i;
  $i ( =i $i            //   i --;
 <loop                  // if ($i) { goto loop; }
 $result                // return result;
```

## Aheui

```aheui
밤밣따빠밣밟따뿌
빠맣파빨받밤뚜뭏
돋밬탕빠맣붏두붇
볻뫃박발뚷투뭏붖
뫃도뫃희멓뭏뭏붘
뫃봌토범더벌뿌뚜
뽑뽀멓멓더벓뻐뚠
뽀덩벐멓뻐덕더벅
```

## Befunge

```befunge
?0|0
0 0  @,,,,,,,,,,,,,_
_0_"!dlroW ,olleH"0|
0                  1
```

## Beatnik

```beatnik
Soars, larkspurs, rains.
Indistinctness.
Mario snarl (nurses, natures, rules...) sensuously retries goal.
Agribusinesses' costs par lain ropes (mopes) autos' cores.
Tuner ambitiousness.
Flit.
Dour entombment.
Legals' saner kinking lapse.
Nests glint.
Dread, tied futures, dourer usual tumor grunts alter atonal
  garb tries shouldered coins.
Taste a vast lustiness.
Stile stuns gad subgroup gram lanes.
Draftee insurer road: cuckold blunt, strut sunnier.
Rely enure pantheism: arty gain groups (genies, pan) titters, tattles, nears.
Bluffer tapes?  Idle diatom stooge!
Feted antes anklets ague?  Remit goiter gout!
Doubtless teared toed alohas will dull gangs' aerials' tails' sluices;
Gusset ends!  Gawkier halo!

Enter abstruse rested loser beer guy louts.
Curtain roams lasso weir lupus stunt.
Truant bears animate talon.  Entire torte originally timer.
Redo stilt gobs.

Utter centaurs;
Urgent stars;
Usurers (dilute);
Noses;
Bones;
Brig sonar graders;
Utensil silts;
Lazies.
Fret arson veterinary rows.

Atlas grunted: "Pates, slues, sulfuric manor liaising tines,
  trailers, rep... unfair!  Instant snots!"

Sled rested until eatery fail.
Ergs fortitude
  Indent spotter
Euros enter egg.
Curious tenures.
Torus cutlasses.
Sarong torso earns cruel lags it reeled.

Engineer: "Erase handbag -- unite ratification!"

oaring oaten donkeys unsold, surer rapid saltest tags
BUTTERED TIBIA LUGS REWIRING TOILETS
anion festers raring edit epilogues.
DIRGE ROTOR.
linnet oaring.
GORE BOOTIES.
Ironed goon lists tallest sublets --
Riots,
Raucous onset.

Ignobly, runners' diet anguishes sunrise loner.
Erode mob, slier switcher!
Loaners stilt drudge pearl atoll, risking hats' ends.

Rebind sitters.

Toga epistles -- crud lard.  (Pager purse dons souls.)

glob title a curio hired rites shed suds lade grease strut arctic revs toad
unless idlers rind stilt region land GERMICIDES SULTANA GUTS gill siting leans
nice spurs
tests gloves
roused asp

Holes!  Moles!  (Sores!)
Hygienists!  Scars!  (Asses!)
Smells spell rares.

Cubs instant sing in parse goodies.
Rosin.  Unhelpful sisal acres.  Slope told.
MALENESS PASTA LAB.  "Infirmary vine," rang illiterates (beans).
Rosin sours, insults truss abalones, nailed rules, helical atlases.
Dear remodeling stings mar rents.
Sunless shiner orb (silly idol.)
Clarity disses senna.
Vagabonds sauted; sloes performed gelds.
Alter post radial lip sectioning gums.
Saint Towellings.
Larger aeons telephone stolid char, pal!
Boats Dean forsook, rosters, tunas, terrariums -- united, traced.
Nude pagoda careens.
```