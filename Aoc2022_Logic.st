
Object subclass: #Aoc2022_Logic
    classInstanceVariableNames: ''
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''!

!Aoc2022_Logic class publicMethods !

sumNumbers: aCollecitonOfNumbers
	"computes a sum by applying the + operator"
	| r |
	r := 0.
	aCollecitonOfNumbers ifNotNil: [
		aCollecitonOfNumbers do: [:e|
			r := r + e.
		].
	].
	^r.	! !

!Aoc2022_Logic publicMethods !

charToN: c
	"helper for day 3"
	| r |
		(((c asInteger) >= ($a asInteger)) & ((c asInteger) <= ($z asInteger))) ifTrue: [ r := (((c asInteger) - ($a asInteger)) + 1) ].
		(((c asInteger) >= ($A asInteger)) & ((c asInteger) <= ($Z asInteger))) ifTrue: [ r := (((c asInteger) - ($A asInteger)) + 27) ].
	^r.!

charToRPS: c
	"converts char to #Rock, #Paper or #Scissors"
	| r n |
	   ((c >= $A) and: (c <= $C)) ifTrue: [ n := (c asInteger) - ($A asInteger)  ].
	   ((c >= $X) and: (c <= $Z)) ifTrue: [ n := (c asInteger) - ($X asInteger)  ].
	   n ifNotNil: [
			r := #(
				#Rock
				#Paper
				#Scissors
			) at: (n + 1).
	   ].
	^r.!

foodPerElf: input
	"given a file with one number per line and empty between groups return the sum of each group"
	| index foodPerElf currentFoodSum |
	foodPerElf := OrderedCollection new.
	index := 0.
	
	currentFoodSum := 0.
	input notNil ifTrue:[ 
	input linesDo: [:line|
		(line size = 0) ifTrue: [
			foodPerElf add: currentFoodSum.
			index := index + 1.
			currentFoodSum := 0.
			]
		ifFalse: [currentFoodSum := currentFoodSum + (line asNumber)  ]
		].
		currentFoodSum ~= 0
			ifTrue: [foodPerElf add: currentFoodSum].
	].
	^foodPerElf.!

pointsForTuple: aRPS_Tuple
	"given a 2 elemement tuple for rock parper scissors
	 compute the score"
	| score myShape otherShape |
	score := 0.
	myShape :=    aRPS_Tuple at: 1.
	otherShape := aRPS_Tuple at: 2.
	
	myShape = #Rock     ifTrue: [ score := score + 1 ].
	myShape = #Paper    ifTrue: [ score := score + 2 ].
	myShape = #Scissors ifTrue: [ score := score + 3 ].
	
	myShape = otherShape
	    ifTrue: [ score := score + 3 ]
	    ifFalse: [
			((((myShape = #Rock) and: (otherShape = #Scissors)) or:
			((myShape = #Paper) and: (otherShape = #Rock)))    or:
			((myShape = #Scissors) and: (otherShape = #Paper)))
			ifTrue: [ score := score + 6].
	    ].
	^score.
	
	
	!

readRucksacks: input
	"does that rucksack scanning thingy"
	| r rucksacks compartments |
	  r := OrderedCollection  new.
	  input linesDo: [:line| 
		| len firstHalf secondHalf midPoint |
		len := line size.
		midPoint := ((len - 1) // 2) + 1.
		firstHalf := line copyFrom: 1 to:  midPoint.
		secondHalf := line copyFrom: midPoint + 1 to: len.
		r  add: { firstHalf. secondHalf }.
	  ].
	^r!

readStrategy: input
	"Reads the strategy guide from aoc 2022 day 2
	 Returns a collection of two element arrays which contain the symbols
	 Rock, Paper or Scissors"
	| r   |
	r := OrderedCollection new.
	
	input lines do: [:l|
		| cols e |
		cols :=  ($ split: l) collect: [:s| self charToRPS: (s at: 1)].
		e := Array  new: 2.
		e at: 1 put: (cols at: 1). 
		e at: 2 put: (cols at: 2).
		r add: e.
	].
	^r!

showAnswer_Day1_1: inputText
	"Given a list of numbers seperated by blank lines find the higest sum of consequtive numbers"
	| tmpFoodPerElf r |
	r := 'No Answer'.
	tmpFoodPerElf := self foodPerElf: inputText.
	tmpFoodPerElf size == 0 ifFalse: [
		r := ((tmpFoodPerElf sort) at: tmpFoodPerElf size) printString.
	].
	^ r.
!

showAnswer_Day1_2: inputText
	| foodPerElf sorted r cpyAmount |
	foodPerElf := self foodPerElf: inputText.
	sorted := (foodPerElf sort).
	cpyAmount := 3.
	sorted size < 3 ifTrue: 
		[ cpyAmount := sorted size ].
		
	sorted size ~= 0 ifTrue: [
		r := sorted copyFrom: sorted size - (cpyAmount - 1) to: sorted size.
	].
	^ ( 'Food: ', ((self class sumNumbers: r)  printString)  ).
!

showAnswer_Day2_1: input
	"does that rock-paper-scissors thing"
	| r strategy score |
	  score := 0.
	  r := ''.
	  strategy := self readStrategy: input.
	  strategy do: [:tuple|
		| thisScore |
		thisScore := (self pointsForTuple: tuple).
		score := score + thisScore.
		r := r, tuple printString, ' ', 16rD asCharacter, (thisScore printString) .
	  ].
	  r :=  (r, score printString).
	^r!

showAnswer_Day2_2: input
	"does that rock-paper-scissors thing"
	| r strategy |
	  strategy := self readStrategy: input.
	^r!

showAnswer_Day3_1: input
	"does that rucksack scanning thingy"
	| r inBoth rucksacks sum |
	r := ''.
	sum := 0.
	  rucksacks := self readRucksacks: input.
	  inBoth := OrderedCollection new.
	  rucksacks do: [:ru |
		"| outStream |
		outStream := WriteStream on: (String new)."
		inBoth addAll:
				((((ru at: 1) intersection: (ru at: 2)) elements)
					select: [:e|  (e notNil)]).
"		inBoth printOn: outStream.
		outStream newLine.
		r := r, (outStream contents). "
	  ].
	  inBoth do: [:c|
			 sum := sum + (self charToN: c).
	  ].
	 
	  r := (sum printString).
	^r .!

showAnswer_Day3_2: input
	"does that rucksack scanning thingy"
	| r rucksacks compartments |
	  rucksacks := self readRucksacks: input.
	^r! !

Aoc2022_Logic initializeAfterLoad!