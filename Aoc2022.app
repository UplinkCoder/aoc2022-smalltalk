

"$COMPATIBLE-DECLARATIONS-START$"
Smalltalk declarePoolDictionary: 'IS_Aoc2022_UI'.
Smalltalk declareVariable: 'IS_instanceInterfaceSpec' poolName: 'IS_Aoc2022_UI'.
"$COMPATIBLE-DECLARATIONS-END$"!

Application create: #Aoc2022 with:
    (#( AbtViewApplication)
        collect: [:each | Smalltalk at: each ifAbsent: [
            Application errorPrerequisite: #Aoc2022 missing: each]])!

Aoc2022 becomeDefault!
AbtAppBldrView subclass: #Aoc2022_UI
    classInstanceVariableNames: ''
    instanceVariableNames: 'numDays controller '
    classVariableNames: ''
    poolDictionaries: ''!

Aoc2022 becomeDefault!
Object subclass: #Aoc2022_Logic
    classInstanceVariableNames: ''
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''!

Aoc2022 becomeDefault!
Application subclass: #Aoc2022
    classInstanceVariableNames: ''
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''!


Aoc2022 becomeDefault!

!Aoc2022 class privateMethods !

abtIsViewApplication

	^true! !

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

!Aoc2022_UI class privateMethods !

_PRAGMA_IS_

	"%%PRAGMA DECLARE
	(name: IS_Aoc2022_UI isPool: true isConstant: false)
	(pool: IS_Aoc2022_UI declarations: (
		(name: IS_instanceInterfaceSpec isConstant: false)
	))"!

IS_instanceInterfaceSpec
	"Private - ** Warning ** This method is generated by VisualAge and should not
	be modified or deleted. This method is responsible for returning a featureSpec
	that describes the implementation of a particular feature of the receiver"

	^IS_Aoc2022_UI::IS_instanceInterfaceSpec notNil
		ifTrue: [IS_Aoc2022_UI::IS_instanceInterfaceSpec]
		ifFalse: [
		IS_Aoc2022_UI::IS_instanceInterfaceSpec := AbtInterfaceSpec new]!

abtPrimFlushInterfaceSpecCache

	IS_Aoc2022_UI associationsDo: [: poolDictionaryAssoc | poolDictionaryAssoc value: nil].
	super abtPrimFlushInterfaceSpecCache!

abtUntranslatedConstants
	"** Do not modify or delete **  See: AbtAppBldrPart class>>#about_abtUntranslatedConstants"

	^#(
	)! !

!Aoc2022_UI publicMethods !

getNumberOfDays
	"because we don't have constants we need to do this blreagh"
	|  |
	^3.!

windowShowing

	| output |

  output := (self subpartNamed: 'OutputText') object.
  output := output asString, '... - ...'.! !

!Aoc2022_UI privateMethods !

abtBuildInternals
	"** Do not modify or delete **  See: AbtAppBldrPart class>>#about_abtBuildInternals"

	| gui window inputText pushButton1 dayList outputText conn0 conn1 |
	gui := self class abtSeparatedConstants.
	window := AbtShellView abtCreatePart: 'Window' parent: nil owner: self .
	inputText := AbtMultiLineEditView abtCreatePart: 'InputText' parent: window.
	pushButton1 := AbtPushButtonView abtCreatePart: 'Push Button1' parent: window.
	dayList := AbtDropDownListComboBox abtCreatePart: 'DayList' parent: window.
	outputText := AbtMultiLineEditView abtCreatePart: 'OutputText' parent: window.
	self 
		 primaryPart: window.
	window 
		 framingSpec: (AbtViewAttachmentConstraint new
			leftEdge: (AbtEdgeConstant new offset: 24);
			rightEdge: (AbtEdgeConstant new offset: 800);
			topEdge: (AbtEdgeConstant new offset: 21);
			bottomEdge: (AbtEdgeConstant new offset: 450));
		 backgroundColor: nil.
	inputText 
		 framingSpec: (AbtViewAttachmentConstraint new
			leftEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 25);
			rightEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHSELFOPPOSITE; offset: 483);
			topEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 19);
			bottomEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHSELFOPPOSITE; offset: 384)).
	pushButton1 
		 framingSpec: (AbtViewAttachmentConstraint new
			leftEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 196);
			rightEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHNONE);
			topEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 406);
			bottomEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHNONE)).
	dayList 
		 framingSpec: (AbtViewAttachmentConstraint new
			leftEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 561);
			rightEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHNONE);
			topEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 20);
			bottomEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHNONE)).
	outputText 
		 framingSpec: (AbtViewAttachmentConstraint new
			leftEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 525);
			rightEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHSELFOPPOSITE; offset: 261);
			topEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHFORM; offset: 143);
			bottomEdge: (AbtRunEdgeAttachmentConstraint new attachment: XmATTACHSELFOPPOSITE; offset: 254)).
	self attributeConnections add:(conn0 := AbtAttributeToCodeHookConnection new
		connectSource: dayList
		featureName: #items
		feature: AbtDropDownListComboBox IS_items
		toTarget: self selector: #dayListItems).
	pushButton1
		abtWhenPrimitive: #clicked
		perform: 
			(DirectedMessage new 
				receiver: self;
				selector: #doDay;
				arguments: #()).
	self initializeAttributeConnections.
	self finalInitialize.
!

dayListItems
	"Creates an entry in the dropdown list for every day."
	| daylist |
	daylist := OrderedCollection new.
	
	2 to: (self getNumberOfDays * 2) do: [:n|
		daylist add: 'Day ', (n // 2) asString, ' ', ((n \\ 2) + 1) asString .
	].
	^daylist.!

doDay
	"Get's called when the go button is pressed"
	| day outputText inputText selectionIndex part text |
		outputText := (self subpartNamed: 'OutputText').
		inputText  := (self subpartNamed: 'InputText') object.
		selectionIndex := (self subpartNamed: 'DayList') selectionIndex.
		day  :=  ((selectionIndex - 1) // 2) + 1.
		part := ((selectionIndex - 1) \\ 2) + 1.
		
		text := (self doDay: day part: part input: inputText).

		outputText object: text.!

doDay: day part: dayPart input: inputText
		controller ifNil: [controller := Aoc2022_Logic new.].
		^controller perform:  (('showAnswer_Day', (day asString), '_', (dayPart asString), ':' ) asSymbol)  with: inputText.!

selectDay: numberOfDay
	(self subpartNamed: 'InputText') enabled: true.! !

Aoc2022 toBeLoadedCode: '"$COMPATIBLE-DECLARATIONS-START$"
Smalltalk declarePoolDictionary: ''IS_Aoc2022_UI''.
Smalltalk declareVariable: ''IS_instanceInterfaceSpec'' poolName: ''IS_Aoc2022_UI''.
"$COMPATIBLE-DECLARATIONS-END$"'!

Aoc2022 wasRemovedCode: '"$COMPATIBLE-DECLARATIONS-START$"
Smalltalk undeclare: ''IS_Aoc2022_UI''.
"$COMPATIBLE-DECLARATIONS-END$"'!

Aoc2022_UI initializeAfterLoad!
Aoc2022_Logic initializeAfterLoad!
Aoc2022 initializeAfterLoad!

Aoc2022 loaded!