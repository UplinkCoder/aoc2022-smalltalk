
AbtAppBldrView subclass: #Aoc2022_UI
    classInstanceVariableNames: ''
    instanceVariableNames: 'numDays controller '
    classVariableNames: ''
    poolDictionaries: ''!

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

Aoc2022_UI initializeAfterLoad!