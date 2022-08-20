import XCTest
@testable import CompilerLib

final class CompileBehaviorTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Behavior", instanceVariables: ["superclass", "methodDict", "format", "subclasses"])
    compiler = Compiler(forClass: classDescription)
  }
  override func tearDown() {
    compiler = nil
    super.tearDown()
  }
  func runningSource(_ source: String, expecting expected: [Int]) throws {
    compiler.compileMethod(source)
    let actual = compiler.context.bytecodes.map { bytecode in bytecode.rawValue }
    XCTAssertEqual(actual.count, expected.count, "Unexpected number of bytecodes")
    let count = min(actual.count, expected.count)
    for i in 0..<count {
      XCTAssertEqual(actual[i], expected[i], "Different bytecodes at position \(i)")
    }
  }

  func testWhichSelectorsAccess() throws {
    let source = """
whichSelectorsAccess: instVarName
	"Answer a set of selectors whose methods access the argument, instVarName,
	as a named instance variable."

	| instVarIndex |
	instVarIndex _ self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^methodDict keys select:
		[:sel |
		((methodDict at: sel)
			readsField: instVarIndex)
			or: [(methodDict at: sel) writesField: instVarIndex]]

	"Point whichSelectorsAccess: 'x'."

"""
    // 17 .. 53
    let expected = [112, 209, 16, 137, 117, 200, 164, 3, 66, 204, 124, 240, 105, 1, 212, 137, 118, 200, 164, 15, 106, 1, 18, 192, 17, 230, 153, 113, 148, 1, 18, 192, 17, 229, 125, 227, 124]
    try runningSource(source, expecting: expected)
  }

  func testRecompileFrom() throws {
    let source = """
recompile: selector from: oldClass
	"Recompile the method associated with selector in the receiver's method dictionary.
	Take care not to write out any new source code - just generate new bytes.
	oldClass may differ from self in order to decompile right (if sourceFiles == nil)
	when adding or removing fields of a class."

	| method trailer methodNode |
	method _ oldClass compiledMethodAt: selector.
	trailer _ (method size - 2 to: method size) collect: [:i | method at: i].
	methodNode _ self compilerClass new
				compile: (oldClass sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [].
	methodNode == nil  "Try again after proceed from SyntaxError"
		ifTrue: [^self recompile: selector].
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	self addSelector: selector withMethod: (methodNode generate: trailer).

"""
    // 27 .. 93
    let expected = [17, 16, 224, 106, 18, 194, 119, 177, 18, 194, 226, 137, 118, 200, 164, 5, 109, 18, 21, 192, 125, 225, 107, 112, 212, 204, 17, 16, 229, 112, 115, 137, 117, 200, 164, 2, 115, 125, 131, 131, 108, 20, 115, 198, 155, 112, 16, 230, 124, 16, 20, 217, 198, 168, 4, 112, 40, 231, 135, 112, 16, 20, 19, 235, 250, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSourceCodeForMethodAt() throws {
    let source = """
sourceCodeForMethod: method at: messageSelector
	"Answer the string corresponding to the source code for the argument."
	| newSource index|
	Sensor leftShiftDown
		ifTrue: [newSource _
					(self decompilerClass new
						decompile: messageSelector
						in: self
						method: method) decompileString]
		ifFalse:
			[newSource _ method getSource.
			newSource == nil
				ifTrue: [newSource _
							(self decompilerClass new
								decompile: messageSelector
								in: self
								method: method) decompileString]
				ifFalse:	[((newSource at: newSource size) isSeparator)
							ifTrue:	[index _ newSource size. "tidy up for file out"
									[((newSource at: index) isSeparator)
										and: [index > 1]]
										whileTrue:	[index _ index - 1].
									newSource _ newSource copyFrom: 1 to: index]]].
	^newSource

"""
    // 19 .. 94
    let expected = [71, 214, 172, 13, 112, 213, 204, 17, 112, 16, 131, 100, 211, 129, 66, 164, 56, 16, 208, 106, 18, 115, 198, 172, 13, 112, 213, 204, 17, 112, 16, 131, 100, 211, 129, 66, 164, 35, 18, 18, 194, 192, 209, 172, 27, 18, 194, 107, 18, 19, 192, 209, 155, 19, 118, 179, 144, 114, 157, 19, 118, 177, 107, 163, 239, 18, 118, 19, 242, 129, 66, 144, 115, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllSubclasses() throws {
    let source = """
allSubclasses
	"Answer an OrderedCollection of the receiver's subclasses and the receiver's ancestor's
	subclasses in breadth-first order, with the immediate subclasses first."
	| coll |
	coll _ OrderedCollection new.
	coll addAll: self subclasses.
	self subclasses do: [:eachSubclass | coll addAll: eachSubclass allSubclasses].
	^coll

"""
    // 11 .. 35
    let expected = [64, 204, 104, 16, 112, 210, 225, 135, 112, 210, 137, 118, 200, 164, 6, 105, 16, 17, 211, 225, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testSourceCodeAt() throws {
    let source = """
sourceCodeAt: messageSelector
	"Answer the string corresponding to the source code for the argument."
	^ self sourceCodeForMethod: (methodDict at: messageSelector) at: messageSelector

"""
    // 5 .. 11
    let expected = [112, 1, 16, 192, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testSourceCodeTemplate() throws {
    let source = """
sourceCodeTemplate
	"Answer an expression to be edited and evaluated in order to
	define methods in this class."

	^'message selector and argument names
	"comment stating purpose of message"

	| temporary variable names |
	statements'

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintHierarchy() throws {
    let source = """
printHierarchy
	"Answer a description containing the names and instance variable
	names of all of the subclasses and superclasses of the receiver."

	| aStream index supers |
	index _ 0.
	aStream _ WriteStream on: (String new: 16).
	self allDynamicSuperclasses reverseDo:
		[:aClass |
		aStream crtab: index.
		index _ index + 1.
		aStream nextPutAll: aClass name.
		aStream space.
		aStream print: aClass instVarNames.
		supers _ aClass superclasses.
		supers size>1 ifTrue:
			[aStream nextPutAll: '  [also a '.
			(supers copyFrom: 2 to: supers size) do:
				[:s | aStream space; nextPutAll: s name; space; print: s allInstVarNames].
			aStream nextPut: $]  ]].
	aStream cr.
	self printSubclassesOn: aStream callingSuperclass: self dynamicSuperclass level: index.
	^aStream contents

"""
    // 45 .. 148
    let expected = [117, 105, 65, 66, 35, 205, 224, 104, 112, 213, 137, 118, 200, 164, 70, 107, 16, 17, 230, 135, 17, 118, 176, 105, 16, 19, 216, 231, 135, 16, 217, 135, 16, 19, 219, 234, 135, 19, 220, 106, 18, 194, 118, 179, 172, 37, 16, 45, 231, 135, 18, 119, 18, 194, 254, 137, 118, 200, 164, 17, 108, 16, 136, 217, 135, 136, 20, 216, 231, 135, 136, 217, 135, 20, 223, 234, 125, 203, 135, 16, 48, 196, 144, 115, 125, 228, 135, 16, 131, 17, 135, 112, 16, 112, 131, 19, 17, 131, 114, 135, 16, 131, 20, 124]
    try runningSource(source, expecting: expected)
  }

  func testShowVariableMenuCollect() throws {
    let source = """
showVariableMenu: generatorBlock collect: valueBlock
	"Construct a menu of variable names supplied by the generatorBlock,
	with lines between classes in the superclass chain.  Show the menu,
	returning the variable chosen by the user, or nil if no
	variable was chosen."

	| eachClass aStream lines count lastLine variables index |
	aStream _ WriteStream on: (String new: 200).
	lines _ OrderedCollection new.
	count _ 0.
	lastLine _ 0.
	variables _ OrderedCollection new.
	self withAllSuperclasses reverseDo:
		[:eachClass |
		count = lastLine ifFalse: [lines add: count.  lastLine _ count].
		(generatorBlock value: eachClass) do:
			[:var |
			aStream nextPutAll: ((valueBlock value: var) contractTo: 20); cr.
			variables addLast: var.
			count _ count + 1]].
	variables isEmpty ifTrue: [^nil].  "Nothing to choose from"
	aStream skip: -1.
	index _ (PopUpMenu labels: aStream contents lines: lines) startUp.
	^index = 0
		ifTrue: [nil]
		ifFalse: [variables at: index]

"""
    // 41 .. 138
    let expected = [65, 66, 35, 205, 224, 107, 68, 204, 108, 117, 109, 117, 110, 68, 204, 111, 112, 214, 137, 118, 200, 164, 45, 106, 21, 22, 182, 168, 6, 20, 21, 231, 135, 21, 110, 16, 18, 202, 137, 118, 200, 164, 23, 130, 73, 19, 136, 17, 25, 202, 42, 233, 232, 135, 219, 135, 23, 25, 236, 135, 21, 118, 176, 129, 69, 125, 203, 125, 229, 135, 23, 221, 152, 123, 19, 116, 238, 135, 81, 19, 131, 18, 20, 131, 80, 223, 130, 72, 24, 117, 182, 153, 115, 146, 23, 24, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllAccessesTo() throws {
    let source = """
allAccessesTo: instVarName
	"Return a list of all methods in my hierarchy that refer to the named instance variable."
	| coll |
	coll _ OrderedCollection new.
	Cursor execute
		showWhile:
			[(self withAllSuperclasses reverse) , self allSubclasses do:
				[:class |
				(class whichSelectorsAccess: instVarName) do:
					[:sel | sel ~~ #DoIt ifTrue: [coll add: class name , ' ' , sel]]]].
	^coll
	"Collection allAccessesTo: 'contents'."

"""
    // 31 .. 85
    let expected = [64, 204, 105, 67, 210, 137, 117, 200, 164, 41, 112, 214, 213, 112, 215, 228, 137, 118, 200, 164, 28, 106, 18, 16, 232, 137, 118, 200, 164, 17, 107, 19, 45, 236, 172, 9, 17, 18, 218, 43, 228, 19, 228, 233, 144, 115, 125, 203, 125, 203, 125, 225, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllCallsOn() throws {
    let source = """
allCallsOn: aLiteral
	"Answer a SortedCollection of all the methods that call on aLiteral."

	| aSortedCollection special |
	aSortedCollection _ SortedCollection new.
	special _ Smalltalk hasSpecialSelector: aLiteral ifTrueSetByte: [:byte ].
	(self withAllSuperclasses reverse) , self allSubclasses do:
		[:class |
		(class whichSelectorsReferTo: aLiteral special: special byte: byte) do:
			[:sel | sel ~~ #DoIt ifTrue: [aSortedCollection add: class name , ' ' , sel]].
		(class class whichSelectorsReferTo: aLiteral special: special byte: byte) do:
			[:sel | sel ~~ #DoIt ifTrue: [aSortedCollection add: class class name , ' ' , sel]].
		].
	^aSortedCollection

"""
    // 29 .. 121
    let expected = [64, 204, 105, 66, 16, 137, 118, 200, 164, 3, 107, 19, 125, 241, 106, 112, 213, 212, 112, 214, 227, 137, 118, 200, 164, 63, 108, 20, 16, 18, 19, 131, 103, 137, 118, 200, 164, 17, 109, 21, 44, 235, 172, 9, 17, 20, 217, 42, 227, 21, 227, 232, 144, 115, 125, 203, 135, 20, 199, 16, 18, 19, 131, 103, 137, 118, 200, 164, 18, 109, 21, 44, 235, 172, 10, 17, 20, 199, 217, 42, 227, 21, 227, 232, 144, 115, 125, 203, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testBrowseAllCallsOn() throws {
    let source = """
browseAllCallsOn: aSymbol
	"Create and schedule a message browser on each method that calls on aSymbol.
	For example,
		Number browseAllCallsOn: #/.	"

	| label key |
	(aSymbol isMemberOf: Association)
		ifTrue: [key _ aSymbol key. 	label _ 'Users of ' , key]
		ifFalse: [key _ aSymbol. 		label _ 'Senders of ', key].

	^ BrowserView
		openListBrowserOn: (self allCallsOn: aSymbol)
		label: label, ' from ', self name
		initialSelection: key asSymbol keywords first

"""
    // 31 .. 69
    let expected = [16, 69, 228, 172, 9, 16, 210, 106, 35, 18, 224, 129, 65, 150, 16, 106, 33, 18, 224, 129, 65, 135, 71, 112, 16, 232, 17, 41, 224, 112, 218, 224, 18, 221, 220, 219, 131, 102, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubclasses() throws {
    let source = """
subclasses
	"Answer the receiver's subclasses.  Return a copy so that callers who
	add or delete subclasses won't get confused."

	subclasses == nil
		ifTrue: [^Set new]
		ifFalse: [^subclasses copy]

"""
    // 7 .. 16
    let expected = [3, 115, 198, 154, 65, 204, 124, 3, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstSize() throws {
    let source = """
instSize
	"Answer the number of named instance variables (as opposed to indexed
	variables) of the receiver."

	^format bitAnd: 255

"""
    // 5 .. 8
    let expected = [2, 32, 190, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubclassDefinerClass() throws {
    let source = """
subclassDefinerClass
	"Return an evaluator class appropriate for evaluating definitions of new
	subclasses of this class."

	^Compiler

"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompileAllFrom() throws {
    let source = """
compileAllFrom: oldClass
	"Compile all the methods in oldClass's method dictionary.
	See recompile:from: regarding oldClass, which is normally just self."

	self selectors do: [:sel | self recompile: sel from: oldClass]

"""
    // 7 .. 22
    let expected = [112, 208, 137, 118, 200, 164, 6, 105, 112, 17, 16, 241, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testBrowseAllAccessesTo() throws {
    let source = """
browseAllAccessesTo: instanceVariable
	"Create and schedule a Message Set browser for all the receiver's methods or any methods of a subclass that refer to the instance variable name.  If the instance variable name is not defined for the receiver, the notification 'Nobody' occurs in the System Transcript."

	BrowserView
			openListBrowserOn: (self allAccessesTo: instanceVariable)
			label: instanceVariable
			initialSelection: instanceVariable

"""
    // 9 .. 18
    let expected = [65, 112, 16, 226, 16, 16, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCheckSuperAddSelector() throws {
    let source = """
checkSuperAddSelector: selector
	| local |
	local _ self includesSelector: selector.
	self hasMultipleSuperclasses
		ifFalse:
			[local ifTrue: [^self].
			^ self subclasses do: [:sub | sub checkSuperAddSelector: selector]].
	(self checkMethodFor: selector) ifFalse:  "Copy or note conflict"
		[Transcript cr; show: 'conflicting methods for ' , selector, ' in ', self name].
	local ifTrue: [^self].  "Was local before, so no change below"
	^ self subclasses do: [:sub | sub checkSuperAddSelector: selector]

"""
    // 27 .. 87
    let expected = [112, 16, 224, 105, 112, 211, 168, 17, 17, 152, 120, 112, 209, 137, 118, 200, 164, 5, 106, 18, 16, 226, 125, 203, 124, 112, 16, 235, 168, 14, 68, 136, 213, 135, 40, 16, 231, 41, 231, 112, 218, 231, 230, 135, 17, 152, 120, 112, 209, 137, 118, 200, 164, 5, 106, 18, 16, 226, 125, 203, 124]
    try runningSource(source, expecting: expected)
  }

  func testHasMultipleSuperclasses() throws {
    let source = """
hasMultipleSuperclasses
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testUpdateInheritanceTables() throws {
    let source = """
updateInheritanceTables: oldSelf
	"I have replaced an old behavior or class.  Update the multiple inheritance tables"
	self updateInheritanceTable: SelectorsOfConflictMethods oldSelf: oldSelf.
	self updateInheritanceTable: SelectorsOfCopiedMethods oldSelf: oldSelf.
	self updateInheritanceTable: SelectorsOfDirectedMethods oldSelf: oldSelf

"""
    // 11 .. 26
    let expected = [112, 65, 16, 240, 135, 112, 66, 16, 240, 135, 112, 67, 16, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testInsertClassSelectorIn() throws {
    let source = """
insertClass: aClass selector: selector in: aDictionary
	| previous |
	previous _ aDictionary at: selector ifAbsent: [Array new].
	(previous includes: aClass) ifFalse:
		[aDictionary at: selector put: (previous copyWith: aClass)]

"""
    // 11 .. 35
    let expected = [18, 17, 137, 117, 200, 164, 3, 65, 204, 125, 240, 107, 19, 16, 227, 168, 7, 18, 17, 19, 16, 226, 193, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAllDynamicSuperclasses() throws {
    let source = """
allDynamicSuperclasses
	"Answer an OrderedCollection of the receiver and the receiver's ancestor's
	dynamic superclasses;  ordered with immediate superclass first."
	| temp |
	superclass == nil
		ifTrue: [^OrderedCollection new]
		ifFalse: [temp _ superclass allDynamicSuperclasses.
				temp addFirst: superclass.
				^temp]

"""
    // 9 .. 24
    let expected = [0, 115, 198, 154, 66, 204, 124, 0, 208, 104, 16, 0, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testWhichSelectorsReferToSpecialByte() throws {
    let source = """
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a collection of selectors whose methods access the argument as a literal."

	| who method methodArray index arraySize |
	who_ OrderedCollection new.
	methodArray _ methodDict methodArray.
	arraySize _ methodArray size.
	index _ 0.
	[(index _ index + 1) <= arraySize] whileTrue:
		[(method _ methodArray at: index) == nil ifFalse:
			[((method refersToLiteral: literal) or:
				[specialFlag and: [method scanFor: specialByte]])
					ifTrue: [who add: (methodDict basicAt: index)]]].
	^who

	"Rectangle whichSelectorsReferTo: #+."

"""
    // 15 .. 67
    let expected = [64, 204, 107, 1, 209, 109, 21, 194, 111, 117, 110, 22, 118, 176, 129, 70, 23, 180, 172, 31, 21, 22, 192, 129, 68, 115, 198, 168, 20, 20, 16, 229, 153, 113, 150, 17, 155, 20, 18, 228, 144, 114, 157, 19, 1, 22, 227, 226, 135, 163, 216, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompoundSelectorsMatching() throws {
    let source = """
compoundSelectorsMatching: simple
	^ self selectors select:
		[:sel | sel isCompound and: [sel selectorPart = simple]]

"""
    // 11 .. 30
    let expected = [112, 209, 137, 118, 200, 164, 11, 105, 17, 211, 156, 17, 210, 16, 182, 144, 114, 125, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testDynamicMethodDescriptionAt() throws {
    let source = """
dynamicMethodDescriptionAt: selector
	"return a method description for the method for 'selector' that would
	 be found by dynamic lookup"
	(methodDict includesKey: selector) ifTrue:
		[^MethodDescription whichClass: self selector: selector].
	superclass == nil ifTrue:
		[^MethodDescription makeMethodNotImplemented].
	^superclass dynamicMethodDescriptionAt: selector

"""
    // 13 .. 32
    let expected = [1, 16, 226, 156, 65, 112, 16, 240, 124, 0, 115, 198, 154, 65, 211, 124, 0, 16, 228, 124]
    try runningSource(source, expecting: expected)
  }


  func testSuperMethodDescriptionAt() throws {
    let source = """
superMethodDescriptionAt: selector
	"return a method description for the method for 'selector' inherited from my superclasses"
	| descr result |
	result _ MethodDescription makeMethodNotImplemented.
	self superclasses do:
		[: each | descr _ each methodDescriptionAt: selector.
		 descr isMethodNotImplemented ifFalse:
		 	[result isMethodNotImplemented
				ifTrue: [result _ descr]
				ifFalse: [result=descr ifFalse:
							[^MethodDescription makeConflictingMethods]]]].
	^result

"""
    // 15 .. 57
    let expected = [65, 208, 106, 112, 210, 137, 118, 200, 164, 29, 107, 19, 16, 227, 105, 17, 213, 154, 115, 164, 17, 18, 213, 156, 17, 129, 66, 164, 9, 18, 17, 182, 153, 115, 146, 65, 212, 124, 125, 203, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveFromInheritanceTables() throws {
    let source = """
removeFromInheritanceTables
	"I have been deleted.  Remove me from multiple inheritance tables"
	self removeFromInheritanceTable: SelectorsOfConflictMethods.
	self removeFromInheritanceTable: SelectorsOfCopiedMethods.
	self removeFromInheritanceTable: SelectorsOfDirectedMethods

"""
    // 11 .. 23
    let expected = [112, 65, 224, 135, 112, 66, 224, 135, 112, 67, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMethodDescriptionAt() throws {
    let source = """
methodDescriptionAt: selector
	"return a method description for the method for 'selector' "
	| local copied conflict |
	local _ methodDict includesKey: selector.
	copied _ (SelectorsOfCopiedMethods at: selector ifAbsent: [Array new]) includes: self.
	conflict _ (SelectorsOfConflictMethods at: selector ifAbsent: [Array new]) includes: self.
	local & copied not & conflict not
		ifTrue: [^MethodDescription whichClass: self selector: selector].
	^self superMethodDescriptionAt: selector

"""
    // 25 .. 73
    let expected = [1, 16, 224, 105, 67, 16, 137, 117, 200, 164, 3, 68, 204, 125, 242, 112, 225, 106, 69, 16, 137, 117, 200, 164, 3, 68, 204, 125, 242, 112, 225, 107, 17, 18, 217, 232, 19, 217, 232, 156, 71, 112, 16, 246, 124, 112, 16, 234, 124]
    try runningSource(source, expecting: expected)
  }

  func testTryCopyingCodeFor() throws {
    let source = """
tryCopyingCodeFor: selector
	"Check if 'selector' is compound, and if so, try to copy down the appropriate code.
	  Return #OK if sucessful,
		#HierarchyViolation if the class part is not one of my immediate superclasses,
		or #NotFound if the class part is OK but the
		selector part is not found in the inheritance hierarchy."
	| classPart whichClass simpleSelector descr |
	selector isCompound ifFalse: [^#NotFound].
	classPart _ selector classPart.
	simpleSelector _ selector selectorPart.
	"check for special class parts"
	classPart==#all ifTrue:
		[self compileBroadcastCodeFor: simpleSelector.
		self insertClass: self selector: simpleSelector in: SelectorsOfDirectedMethods.
		^#OK].
	classPart==#super
		ifTrue: [descr _ self superMethodDescriptionAt: simpleSelector]
		ifFalse: [whichClass _ Smalltalk at: classPart.
				"if I'm a metaclass, get the metaclass of whichClass"
				self isMeta ifTrue: [whichClass _ whichClass class].
				"check that whichClass is one of my superclasses"
				(self inheritsFrom: whichClass) ifFalse: [^#HierarchyViolation].
				descr _ whichClass methodDescriptionAt: simpleSelector].
	descr isBad ifTrue: [^#NotFound].
	self compileUnchecked: classPart , '.' , descr sourceCode.
	self insertClass: self selector: simpleSelector in: SelectorsOfDirectedMethods.
	^#OK

"""
    // 45 .. 136
    let expected = [16, 209, 168, 2, 32, 124, 16, 210, 105, 16, 211, 107, 17, 40, 198, 172, 13, 112, 19, 228, 135, 112, 112, 19, 70, 131, 101, 135, 39, 124, 17, 47, 198, 158, 112, 19, 238, 129, 68, 164, 22, 73, 17, 192, 106, 112, 218, 154, 18, 199, 106, 112, 18, 236, 168, 2, 43, 124, 18, 19, 237, 129, 68, 135, 20, 131, 16, 153, 32, 124, 112, 17, 51, 131, 50, 20, 131, 20, 131, 50, 131, 49, 135, 112, 112, 19, 70, 131, 101, 135, 39, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompileUnchecked() throws {
    let source = """
compileUnchecked: code
	"Compile the argument, code, and install the result in the receiver's method dictionary.
	Do not check for possible effect on inheritance, since that's what this is doing."
	| selector methodNode |
	methodNode _ self compilerClass new
				compile: code
				in: self
				notifying: nil
				ifFail: [^nil].
	selector _ methodNode selector.
	self addSelectorUnchecked: selector withMethod: (methodNode generate: #(0 0 0)).
	^selector

"""
    // 15 .. 41
    let expected = [112, 209, 204, 16, 112, 115, 137, 117, 200, 164, 1, 123, 131, 128, 106, 18, 210, 105, 112, 17, 18, 37, 228, 243, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testCheckChangeSelector() throws {
    let source = """
checkChangeSelector: selector
	| descr classes class |
	"The method for selector has been changed or removed.
	 Check all copied versions for the method in question."
	(SelectorsOfCopiedMethods at: selector ifAbsent: [Array new]) do:
		[:class |
		(class inheritsFrom: self) ifTrue:
			[(class checkMethodFor: selector) ifFalse:
				[Transcript cr; show: 'conflicting methods for ' , selector, ' in ', class name]
				]].
	"Remove all versions copied for directed access (eg Point.max) "
	(SelectorsOfDirectedMethods at: selector ifAbsent: [Array new]) do:
		[:class |
		(class inheritsFrom: self) ifTrue:
			[(class compoundSelectorsMatching: selector) do:
				[:sel | class removeSelectorUnchecked: sel].
		self removeClass: class selector: selector in: SelectorsOfDirectedMethods]]

"""
    // 35 .. 130
    let expected = [65, 16, 137, 117, 200, 164, 3, 66, 204, 125, 240, 137, 118, 200, 164, 29, 107, 19, 112, 235, 172, 21, 19, 16, 234, 154, 115, 164, 13, 67, 136, 212, 135, 39, 16, 230, 40, 230, 19, 217, 230, 229, 144, 115, 125, 203, 135, 76, 16, 137, 117, 200, 164, 3, 66, 204, 125, 240, 137, 118, 200, 164, 30, 107, 19, 112, 235, 172, 22, 19, 16, 237, 137, 118, 200, 164, 5, 108, 19, 20, 238, 125, 203, 135, 112, 19, 16, 76, 131, 111, 144, 115, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCheckMethodFor() throws {
    let source = """
checkMethodFor: selector
	"copy method from superclass if necessary.  Answer true if no conflict detected"
	| descr unmoved copyOK local copied conflict |
	local _ methodDict includesKey: selector.
	copied _ (SelectorsOfCopiedMethods at: selector ifAbsent: [Array new]) includes: self.
	conflict _ (SelectorsOfConflictMethods at: selector ifAbsent: [Array new]) includes: self.
	local & copied not & conflict not
		ifTrue: [^true].
	descr _ self superMethodDescriptionAt: selector.
	descr isMethodNotImplemented
		ifTrue: [self removeSelectorUnchecked: selector.  ^true].
	descr isConflictingMethods
		ifTrue: [self compileConflictCodeFor: selector.  ^false].

	conflict ifTrue:  "Not conflicting any more, so remove if it had been."
		[self removeSelectorUnchecked: selector.
		self removeClass: self selector: selector in: SelectorsOfConflictMethods].

	"If this method isnt on the dynamic chain, copy it."
	descr = (self dynamicMethodDescriptionAt: selector) ifFalse:
		[unmoved _ self unmovedVarsFrom: descr whichClass.
		copyOK _ true.  "If method doesnt touch any vars which moved"
		descr method fieldsTouched do:
			[:field | copyOK _ copyOK & (unmoved at: field)].
		copyOK
			ifTrue:  "then can just install that same method"
				[self addSelectorUnchecked: descr selector withMethod: descr method]
			ifFalse:  "otherwise have to recompile it here"
				[self compileUnchecked: descr sourceCode].
		self insertClass: self selector: selector in: SelectorsOfCopiedMethods].
	^true

"""
    // 51 .. 189
    let expected = [1, 16, 224, 108, 67, 16, 137, 117, 200, 164, 3, 68, 204, 125, 242, 112, 225, 109, 69, 16, 137, 117, 200, 164, 3, 68, 204, 125, 242, 112, 225, 110, 20, 21, 215, 230, 22, 215, 230, 152, 121, 112, 16, 232, 105, 17, 218, 156, 112, 16, 233, 135, 121, 17, 220, 156, 112, 16, 235, 135, 122, 22, 172, 11, 112, 16, 233, 135, 112, 112, 16, 69, 131, 109, 135, 17, 112, 16, 131, 55, 182, 168, 55, 112, 17, 223, 238, 106, 113, 107, 17, 131, 17, 131, 16, 137, 118, 200, 164, 9, 111, 19, 18, 23, 192, 230, 129, 67, 125, 203, 135, 19, 172, 10, 112, 17, 131, 21, 17, 131, 17, 131, 84, 149, 112, 17, 131, 19, 131, 50, 135, 112, 112, 16, 67, 131, 118, 135, 121]
    try runningSource(source, expecting: expected)
  }

  func testCompileBroadcastCodeFor() throws {
    let source = """
compileBroadcastCodeFor: selector
	"compile code that invokes ALL methods for 'selector' in my inheritance hierarchy"
	| implementors strm keywords argNames |
	implementors _ self withAllSuperclasses select:
		[:each | each includesSelector: selector].
	argNames _ Array new: selector numArgs.
	1 to: argNames size do: [:i | argNames at: i put: 'arg' , i printString].
	strm _ WriteStream on: (String new: 500).
	strm nextPutAll: 'all.'.
	argNames size=0
		ifTrue: [strm nextPutAll: selector]
		ifFalse: [keywords _ selector keywords.
				1 to: argNames size do:
					[:i | strm nextPutAll: (keywords at: i); space;
						nextPutAll: (argNames at: i); space]].
	implementors do:
		[:each | strm cr; tab; nextPutAll: 'self '; nextPutAll: each name; nextPut: $. .
			argNames size=0
				ifTrue: [strm nextPutAll: selector]
				ifFalse: [keywords _ selector keywords.
						1 to: argNames size do:
							[:i | strm nextPutAll: (keywords at: i); space;
								nextPutAll: (argNames at: i); space]].
			strm nextPut: $.].
	self compileUnchecked: strm contents

"""
    // 51 .. 229
    let expected = [112, 209, 137, 118, 200, 164, 5, 109, 21, 16, 226, 125, 224, 105, 67, 16, 212, 205, 108, 118, 20, 194, 137, 118, 200, 164, 9, 110, 20, 22, 39, 22, 216, 230, 193, 125, 245, 135, 74, 75, 44, 205, 233, 106, 18, 46, 237, 135, 20, 194, 117, 182, 156, 18, 16, 237, 164, 33, 16, 223, 107, 118, 20, 194, 137, 118, 200, 164, 21, 110, 18, 136, 19, 22, 192, 237, 135, 136, 131, 16, 135, 136, 20, 22, 192, 237, 135, 131, 16, 125, 245, 135, 17, 137, 118, 200, 164, 71, 109, 18, 136, 131, 17, 135, 136, 131, 18, 135, 136, 51, 237, 135, 136, 21, 131, 20, 237, 135, 53, 196, 135, 20, 194, 117, 182, 156, 18, 16, 237, 164, 33, 16, 223, 107, 118, 20, 194, 137, 118, 200, 164, 21, 110, 18, 136, 19, 22, 192, 237, 135, 136, 131, 16, 135, 136, 20, 22, 192, 237, 135, 131, 16, 125, 245, 135, 18, 53, 196, 125, 203, 135, 112, 18, 131, 23, 131, 54, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNewSized() throws {
    let source = """
new: anInteger
	"Answer a new instance of the receiver (which is a class) with the number of
	indexable variables specified by the argument, anInteger.  Fail if the class is not
	indexable or if the argument is not a positive Integer.  Essential.  See Object
	documentation whatIsAPrimitive."

	<primitive: 71>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPoolHasIfTrue() throws {
    let source = """
poolHas: varName ifTrue: assocBlock
	"Behaviors have no pools"
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testCopyMethods() throws {
    let source = """
copyMethods  "copy all methods from superclasses not on the dynamic lookup chain"
	| noConflicts |
	noConflicts _ true.
	self allSelectors do:
		[:selector | noConflicts _ noConflicts & (self checkMethodFor: selector)].
	noConflicts ifFalse:
		[Transcript cr; show: self name , ' has conflicting inherited methods
  -- consult browser for their names']

"""
    // 21 .. 54
    let expected = [113, 104, 112, 208, 137, 118, 200, 164, 9, 105, 16, 112, 17, 226, 225, 129, 64, 125, 203, 135, 16, 168, 10, 67, 136, 212, 135, 112, 215, 40, 230, 229, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRecompile() throws {
    let source = """
recompile: selector
	^ self recompile: selector from: self

"""
    // 5 .. 9
    let expected = [112, 16, 112, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllSelectors() throws {
    let source = """
allSelectors
	"Answer a set of all the message selectors that instances of the receiver can
	understand."
	| aSet |
	aSet _ Set new.
	self withAllSuperclasses do: [:each | aSet addAll: each selectors].
	^aSet

	"Point allSelectors."

"""
    // 11 .. 30
    let expected = [64, 204, 104, 112, 209, 137, 118, 200, 164, 6, 105, 16, 17, 211, 226, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testFormatVariableWordsPointers() throws {
    let source = """
format: nInstVars variable: isVar words: isWords pointers: isPointers
	"Set the format for the receiver (a Class)."

	format _ nInstVars +
				(isVar
						ifTrue: [4096]
						ifFalse: [0]) +
				(isWords
						ifTrue: [8192]
						ifFalse: [0]) +
				(isPointers
						ifTrue: [-16384]
						ifFalse: [0])

"""
    // 9 .. 29
    let expected = [16, 17, 153, 32, 144, 117, 176, 18, 153, 33, 144, 117, 176, 19, 153, 34, 144, 117, 176, 98, 120]
    try runningSource(source, expecting: expected)
  }

  func testAllClassVarNames() throws {
    let source = """
allClassVarNames
	"Answer a Set of the names of the receiver's and the receiver's
	ancestor's class variables."

	^superclass allClassVarNames

"""
    // 5 .. 7
    let expected = [0, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testBasicNewSized() throws {
    let source = """
basicNew: anInteger
	"Answer a new instance of the receiver (which is a class) with the number of
	indexable variables specified by the argument, anInteger.  Fail if the class is not
	indexable or if the argument is not a positive Integer.  Essential.  See Object
	documentation whatIsAPrimitive."

	<primitive: 71>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAllSharedPools() throws {
    let source = """
allSharedPools
	"Answer a Set of the pools, dictionaries, that the receiver and the
	receiver's ancestors share.  Subclasses, such as class Class, override this message."

	^superclass allSharedPools

"""
    // 5 .. 7
    let expected = [0, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testCrossReference() throws {
    let source = """
crossReference
	"Answer an array of arrays of size 2 whose first element is a message selector
	in the receiver's method dictionary and whose second element is a set of all message
	selectors in the method dictionary whose methods send a message with that selector.
	Subclasses are not included."

	^self selectors asSortedCollection asArray collect:
		[:x |
		Array
			with: (String with: Character cr), x
			with: (self whichSelectorsReferTo: x)]

	"Point crossReference."

"""
    // 27 .. 50
    let expected = [112, 211, 210, 209, 137, 118, 200, 164, 13, 104, 69, 72, 74, 217, 231, 16, 230, 112, 16, 235, 244, 125, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetSuperclass() throws {
    let source = """
superclass: aClass
	"Change the receiver's superclass to be aClass."

	(aClass isKindOf: Behavior)
		ifTrue: [superclass _ aClass]
		ifFalse: [self error: 'superclass must be a class-describing object']

"""
    // 11 .. 23
    let expected = [16, 67, 226, 155, 16, 129, 0, 146, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testKindOfSubclass() throws {
    let source = """
kindOfSubclass
	"Answer a string which is the keyword that describes the receiver's kind of
	subclass, either a regular subclass, a variableSubclass, a variableByteSubclass, or
	a variableWordSubclass."

	self isVariable
		ifTrue: [self isBits
					ifTrue: [self isBytes
								ifTrue: [^' variableByteSubclass: ']
								ifFalse: [^' variableWordSubclass: ']]
					ifFalse: [^' variableSubclass: ']]
		ifFalse: [^' subclass: ']

"""
    // 17 .. 34
    let expected = [112, 214, 172, 12, 112, 213, 158, 112, 212, 153, 35, 124, 34, 124, 33, 124, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testSomeInstance() throws {
    let source = """
someInstance
	"Answer the first instance of this receiver.  See Object nextInstance.  Fails
	if there are none.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 77>
	^nil

"""
    // 7 .. 7
    let expected = [123]
    try runningSource(source, expecting: expected)
  }

  func testCanUnderstand() throws {
    let source = """
canUnderstand: selector
	"Answer true if the receiver can respond to the message whose selector
	is the argument, false otherwise.  The selector can be in the method dictionary
	of the receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^true].
	superclass == nil ifTrue: [^false].
	^superclass canUnderstand: selector

"""
    // 7 .. 20
    let expected = [112, 16, 224, 152, 121, 0, 115, 198, 152, 122, 0, 16, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintMethodChunkOnMoveSourceToFile() throws {
    let source = """
printMethodChunk: selector on: aFileStream moveSource: moveSource toFile: fileIndex
	"Print the source code for the method associated with the argument selector onto
	the fileStream. aFileStream, and, for backup, if the argument moveSource (a Boolean)
	is true, also set the file index within the method to be the argument fileIndex."

	| position |
	aFileStream cr; cr.
	moveSource ifTrue: [position _ aFileStream position].
	aFileStream nextChunkPut: (self sourceMethodAt: selector) asString.
	moveSource
		ifTrue: [(self compiledMethodAt: selector)
					setSourcePosition: position inFile: fileIndex]

"""
    // 17 .. 44
    let expected = [17, 136, 208, 135, 208, 135, 18, 154, 17, 209, 108, 17, 112, 16, 228, 211, 226, 135, 18, 158, 112, 16, 230, 20, 19, 245, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMethodDictionary() throws {
    let source = """
methodDictionary: aDictionary
	"Store the argument, aDictionary, as the method dictionary of the receiver."

	methodDict _ aDictionary

"""
    // 3 .. 5
    let expected = [16, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testDecompile() throws {
    let source = """
decompile: selector
	"Find the compiled code associated with the argument, selector, as a message selector
	in the receiver's method dictionary and decompile it.  Answer the resulting source
	code as a string.  Create an error if the selector is not in the receiver's method
	dictionary."

	^self decompilerClass new decompile: selector in: self

"""
    // 7 .. 13
    let expected = [112, 209, 204, 16, 112, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testHasMethods() throws {
    let source = """
hasMethods
	"Answer whether the receiver has any methods in its method dictionary."

	^methodDict size > 0

"""
    // 3 .. 7
    let expected = [1, 194, 117, 179, 124]
    try runningSource(source, expecting: expected)
  }

  func testNew() throws {
    let source = """
new
	"Answer a new instance of the receiver (which is a class) with no indexable
	variables.  Fail if the class is indexable.  Essential.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 70>
	self isVariable ifTrue: [^self new: 0].
	self primitiveFailed

"""
    // 11 .. 21
    let expected = [112, 208, 155, 112, 117, 205, 124, 112, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCompileNotifyingTrailer() throws {
    let source = """
compile: code notifying: requestor trailer: bytes
	"Compile  the argument, code, as source code in the context of the
	receiver.  Use the default fail code [^nil].  Does not save source code.
	The second argument, requestor, is to be notified if an error occurs. The
	argument code is either a string or an object that converts to a string or a
	PositionableStream on an object that converts to a string. The third argument,
	bytes, is a trailer, that is, an array of three bytes that should be added to the end
	of the compiled method. These point to the location of the source code (on a file)."

	^self compile: code notifying: requestor trailer: bytes ifFail: [^nil]

"""
    // 5 .. 17
    let expected = [112, 16, 17, 18, 137, 117, 200, 164, 1, 123, 131, 128, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllSuperclasses() throws {
    let source = """
allSuperclasses
	"Answer an OrderedCollection of the receiver's superclasses and the receiver's ancestor's
	superclasses in breadth-first order, with the immediate superclasses first."
	| coll |
	coll _ OrderedCollection new.
	self allSuperclassesInto: coll.
	^coll

"""
    // 7 .. 15
    let expected = [64, 204, 104, 112, 16, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsFixed() throws {
    let source = """
isFixed
	"Answer whether the receiver does not have a variable (indexable) part."

	^self isVariable not

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompileNotifyingTrailerIfFail() throws {
    let source = """
compile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile the argument, code, as source code in the context of the receiver and
	install the result in the receiver's method dictionary.  The argument requestor is to
	be notified if an error occurs. The argument code is either a string or an
	object that converts to a string or a PositionableStream on an object that
	converts to a string.  The trailer is an array of three bytes that should
	be added to the end of the compiled method.  These point to the location
	of the source code (on a file).   This method does not save the source code.
	Evaluate the failBlock if the compilation does not succeed."

	| methodNode selector |
	methodNode _ self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	selector _ methodNode selector.
	self addSelector: selector withMethod: (methodNode generate: bytes).
	^selector

"""
    // 13 .. 34
    let expected = [112, 209, 204, 16, 112, 17, 19, 131, 128, 108, 20, 210, 109, 112, 21, 20, 18, 228, 243, 135, 21, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllInstances() throws {
    let source = """
allInstances
	"Answer a collection of all instances of this class."

	| aCollection |
	aCollection _ OrderedCollection new.
	self allInstancesDo:
		[:x | x == aCollection ifFalse: [aCollection add: x]].
	^aCollection

"""
    // 9 .. 32
    let expected = [64, 204, 104, 112, 137, 118, 200, 164, 11, 105, 17, 16, 198, 153, 115, 146, 16, 17, 226, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsPointers() throws {
    let source = """
isPointers
	"Answer whether the receiver contains just pointers (not bits)."

	^self isBits not

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testDecompilerClass() throws {
    let source = """
decompilerClass
	"Return a decompiler class appropriate for compiled methods of this class."

	^Decompiler

"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompileAllSubclasses() throws {
    let source = """
compileAllSubclasses
	"Compile all the methods in the receiver's subclasses.  This does not modify
	code (re-install the compiled versions), just compiles the methods as a kind of static
	check."

	self allSubclasses do: [:aSubclass | aSubclass compileAll]

"""
    // 7 .. 20
    let expected = [112, 208, 137, 118, 200, 164, 4, 104, 16, 209, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSubclassInstVarNames() throws {
    let source = """
subclassInstVarNames
	"Answer a Set of the names of the receiver's subclasses' instance variables."
	| vars |
	vars _ Set new.
	self allSubclasses do: [:aSubclass | vars addAll: aSubclass instVarNames].
	^vars

"""
    // 11 .. 30
    let expected = [64, 204, 104, 112, 209, 137, 118, 200, 164, 6, 105, 16, 17, 211, 226, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstanceCount() throws {
    let source = """
instanceCount
	"Answer the number of instances of the receiver that are currently in use."

	| count |
	count _ 0.
	self allInstancesDo: [:x | count _ count + 1].
	^count

"""
    // 5 .. 23
    let expected = [117, 104, 112, 137, 118, 200, 164, 7, 105, 16, 118, 176, 129, 64, 125, 224, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllInstancesDo() throws {
    let source = """
allInstancesDo: aBlock
	"Evaluate the argument, aBlock, for each of the current instances of the receiver."

	| inst next |
	inst _ self someInstance.
	inst == nil
		ifFalse:
			[[next _ inst nextInstance.
			aBlock value: inst.
			next == nil]
				whileFalse: [inst _ next]].
	nil class == self ifTrue: [aBlock value: nil]

"""
    // 7 .. 40
    let expected = [112, 208, 105, 17, 115, 198, 168, 16, 17, 209, 106, 16, 17, 202, 135, 18, 115, 198, 168, 4, 18, 105, 163, 240, 115, 199, 112, 198, 155, 16, 115, 202, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddSelectorUncheckedWithMethod() throws {
    let source = """
addSelectorUnchecked: selector withMethod: compiledMethod
	"Add the message selector with the corresponding compiled method to the receiver's
	method dictionary.  Do not check for effect on (multiple) inheritance."

	methodDict at: selector put: compiledMethod.
	self flushCache

"""
    // 5 .. 13
    let expected = [1, 16, 17, 193, 135, 112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testWhichSelectorsReferTo() throws {
    let source = """
whichSelectorsReferTo: literal
	"Answer a set of selectors whose methods access the argument as a literal."

	| special |
	special _ Smalltalk hasSpecialSelector: literal ifTrueSetByte: [:byte ].
	^self whichSelectorsReferTo: literal special: special byte: byte

	"Rectangle whichSelectorsReferTo: #+."

"""
    // 9 .. 27
    let expected = [65, 16, 137, 118, 200, 164, 3, 106, 18, 125, 240, 105, 112, 16, 17, 18, 131, 98, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompileAll() throws {
    let source = """
compileAll
	^ self compileAllFrom: self

"""
    // 5 .. 8
    let expected = [112, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllSubInstancesDo() throws {
    let source = """
allSubInstancesDo: aBlock
	"Evaluate the argument, aBlock, for each of the current instances of the receiver's
	subclasses."

	self allSubclassesDo: [:sub | sub allInstancesDo: aBlock]

"""
    // 7 .. 20
    let expected = [112, 137, 118, 200, 164, 5, 105, 17, 16, 225, 125, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFlushCache() throws {
    let source = """
flushCache
	"Tell the interpreter to remove the contents of its method lookup cache, if it has
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testInheritsFrom() throws {
    let source = """
inheritsFrom: aClass
	"Answer whether the argument, aClass, is on the receiver's superclass chain."
	self superclasses do:
		[:each | (each==aClass or: [each inheritsFrom: aClass]) ifTrue: [^true]].
	^false

"""
    // 7 .. 30
    let expected = [112, 208, 137, 118, 200, 164, 14, 105, 17, 16, 198, 153, 113, 146, 17, 16, 225, 152, 121, 115, 125, 203, 135, 122]
    try runningSource(source, expecting: expected)
  }

  func testIsBytes() throws {
    let source = """
isBytes
	"Answer whether the receiver has 8-bit instance variables."
	^format noMask: 8192

"""
    // 7 .. 10
    let expected = [2, 33, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsBits() throws {
    let source = """
isBits
	"Answer whether the receiver contains just bits (not pointers)."

	^format noMask: -16384

"""
    // 7 .. 10
    let expected = [2, 33, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSelectors() throws {
    let source = """
selectors
	"Answer a Set of all the message selectors specified in the receiver's
	method dictionary."

	^methodDict keys

	"Point selectors."

"""
    // 5 .. 7
    let expected = [1, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testSelectorAtMethodSetClass() throws {
    let source = """
selectorAtMethod: method setClass: classResultBlock
	"Answer both the message selector associated with the compiled method
	and the class in which that selector is defined."

	| sel |
	sel _ methodDict keyAtValue: method
				ifAbsent:
					[superclass == nil
						ifTrue:
							[classResultBlock value: self.
							^self defaultSelectorForMethod: method].
					sel _ superclass selectorAtMethod: method setClass: classResultBlock.
					"Set class to be self, rather than that returned from
					superclass. "
					sel == (self defaultSelectorForMethod: method) ifTrue: [classResultBlock value: self].
					^sel].
	classResultBlock value: self.
	^sel

"""
    // 9 .. 52
    let expected = [1, 16, 137, 117, 200, 164, 29, 0, 115, 198, 159, 17, 112, 202, 135, 112, 16, 225, 124, 0, 16, 17, 242, 106, 18, 112, 16, 225, 198, 155, 17, 112, 202, 135, 18, 124, 240, 106, 17, 112, 202, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveSelectorUnchecked() throws {
    let source = """
removeSelectorUnchecked: selector
	"Assuming that the message selector is in the receiver's method dictionary,
	remove it.  If the selector is not in the method dictionary, create an error
	notification.  Do not check for effect on (multiple) inheritance."

	methodDict removeKey: selector.
	self flushCache

"""
    // 7 .. 14
    let expected = [1, 16, 224, 135, 112, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSourceTextAt() throws {
    let source = """
sourceTextAt: selector
	"Answer with the string of the source code for the message selector."

	| newSource method |
	method _ methodDict at: selector.
	Sensor leftShiftDown
		ifTrue: [newSource _
					self decompilerClass new
						decompile: selector
						in: self
						method: method]
		ifFalse:
			[newSource _ method getSource.
			newSource == nil
				ifTrue: [newSource _
							self decompilerClass new
								decompile: selector
								in: self
								method: method]].
	^newSource asText

"""
    // 15 .. 58
    let expected = [1, 16, 192, 106, 68, 211, 172, 12, 112, 210, 204, 16, 112, 18, 131, 97, 129, 65, 164, 20, 18, 208, 105, 17, 115, 198, 172, 11, 112, 210, 204, 16, 112, 18, 131, 97, 129, 65, 144, 115, 135, 17, 213, 124]
    try runningSource(source, expecting: expected)
  }

  func testDefaultSelectorForMethod() throws {
    let source = """
defaultSelectorForMethod: aMethod
	"Given a method, invent an appropriate selector, that is, one that will parse with
	the correct number of arguments."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	aStream nextPutAll: 'unboundMethod'.
	1 to: aMethod numArgs do: [:i | aStream nextPutAll: 'with:'].
	^aStream contents asSymbol

"""
    // 25 .. 53
    let expected = [65, 66, 35, 205, 224, 105, 17, 37, 228, 135, 118, 16, 215, 137, 118, 200, 164, 5, 106, 17, 40, 228, 125, 246, 135, 17, 218, 217, 124]
    try runningSource(source, expecting: expected)
  }

  func testObsolete() throws {
    let source = """
obsolete
	"Invalidate and recycle local messages.  Remove the receiver from its superclass'
	subclass list."

	methodDict _ MethodDictionary new.
	self superclasses do: [:each | each removeSubclass: self].
	self removeFromInheritanceTables

"""
    // 11 .. 31
    let expected = [64, 204, 97, 112, 209, 137, 118, 200, 164, 5, 104, 16, 112, 226, 125, 203, 135, 112, 211, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAllSubclassesDo() throws {
    let source = """
allSubclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclasses do:
		[:cl |
		aBlock value: cl.
		cl allSubclassesDo: aBlock]

"""
    // 7 .. 25
    let expected = [112, 208, 137, 118, 200, 164, 9, 105, 16, 17, 202, 135, 17, 16, 225, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveSelector() throws {
    let source = """
removeSelector: selector
	"Assuming that the message selector is in the receiver's method dictionary,
	remove it.  If the selector is not in the method dictionary, create an error
	notification."

	methodDict removeKey: selector.
	self flushCache.
	self checkChangeSelector: selector

"""
    // 9 .. 20
    let expected = [1, 16, 224, 135, 112, 209, 135, 112, 16, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveSubclass() throws {
    let source = """
removeSubclass: aSubclass
	"If the argument, aSubclass, is one of the receiver's subclasses, remove it."
	subclasses == nil ifFalse:
		[subclasses remove: aSubclass ifAbsent: [].
		subclasses isEmpty ifTrue: [subclasses _ nil]]

"""
    // 7 .. 28
    let expected = [3, 115, 198, 168, 16, 3, 16, 137, 117, 200, 164, 2, 115, 125, 240, 135, 3, 209, 153, 115, 99, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddSubclass() throws {
    let source = """
addSubclass: aSubclass
	"Make the argument, aSubclass, be one of the subclasses of the receiver."

	(aSubclass superclasses includes: self)
		ifTrue: [subclasses == nil
					ifTrue:	[subclasses _ Set with: aSubclass]
					ifFalse:	[subclasses add: aSubclass]]
		ifFalse: [self error: aSubclass name , ' is not my subclass']

"""
    // 21 .. 48
    let expected = [16, 216, 112, 231, 172, 14, 3, 115, 198, 157, 70, 16, 229, 129, 3, 146, 3, 16, 228, 149, 112, 16, 210, 35, 225, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testWhichClassIncludesSelector() throws {
    let source = """
whichClassIncludesSelector: aSymbol
	"Answer the class on the receiver's superclass chain where the argument, aSymbol
	(a message selector), will be found."

	(methodDict includesKey: aSymbol) ifTrue: [^self].
	superclass == nil ifTrue: [^nil].
	^superclass whichClassIncludesSelector: aSymbol

	"Rectangle whichClassIncludesSelector: #inspect."

"""
    // 7 .. 20
    let expected = [1, 16, 224, 152, 120, 0, 115, 198, 152, 123, 0, 16, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllSuperclassesInto() throws {
    let source = """
allSuperclassesInto: orderedCollection
	"Add all my superclasses to orderedCollection if not already there.
	  Use breadth-first order."
	| mysupers |
	mysupers _ self superclasses.
	mysupers do: [:each | each allSuperclassesInto: orderedCollection].
	mysupers reverseDo:
		[:each | (orderedCollection includes: each) ifFalse: [orderedCollection addFirst: each]]

"""
    // 13 .. 48
    let expected = [112, 208, 105, 17, 137, 118, 200, 164, 5, 106, 18, 16, 225, 125, 203, 135, 17, 137, 118, 200, 164, 11, 106, 16, 18, 228, 153, 115, 146, 16, 18, 227, 125, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSourceMethodAt() throws {
    let source = """
sourceMethodAt: selector
	"Answer the paragraph corresponding to the source code for the argument."

	^(self sourceCodeAt: selector) asText makeSelectorBoldIn: self

"""
    // 9 .. 15
    let expected = [112, 16, 226, 209, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream

"""
    // 9 .. 17
    let expected = [16, 33, 224, 135, 0, 16, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSuperclasses() throws {
    let source = """
superclasses
	"Answer with an array of all the receiver's superclasses."
	superclass == nil ifTrue: [^#()].
	self hasMultipleSuperclasses
		ifTrue: [^ (Array with: superclass) , self class otherSuperclasses].
	^ Array with: superclass

"""
    // 15 .. 35
    let expected = [0, 115, 198, 153, 32, 124, 112, 213, 159, 67, 0, 226, 112, 199, 212, 225, 124, 67, 0, 226, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsVariable() throws {
    let source = """
isVariable
	"Answer whether the receiver has a variable (indexable) part."

	^(format bitAnd: 4096) ~= 0

"""
    // 5 .. 10
    let expected = [2, 32, 190, 117, 183, 124]
    try runningSource(source, expecting: expected)
  }

  func testConflictCodeFor() throws {
    let source = """
conflictCodeFor: sel  "return some code that indicates a conflicting definition"
	| code parser |
	code _ (self dynamicMethodDescriptionAt: sel) sourceCode.
	(parser _ self parserClass new) parseSelector: code.
	^ (code copyFrom: 1 to: (parser endOfLastToken min: code size)) ,
		(String with: Character cr) ,
		'	^self conflictingInheritanceError'

"""
    // 29 .. 57
    let expected = [112, 16, 225, 208, 105, 112, 211, 204, 129, 66, 17, 226, 135, 17, 118, 18, 215, 17, 194, 230, 245, 73, 75, 218, 232, 228, 44, 228, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintSubclassesOnCallingSuperclassLevel() throws {
    let source = """
printSubclassesOn: aStream callingSuperclass: whichSuper level: level
	"As part of the algorithm for printing a description of the receiver, print the
	subclass on the file stream, aStream, indenting level times."
	| subs supers |
	aStream crtab: level.
	aStream nextPutAll: self name.
	aStream space; print: self instVarNames.
	supers _ self superclasses.
	supers size>1 ifTrue:
		[aStream nextPutAll: '  [also a'.
		(supers copyWithout: whichSuper) do:
			[:s | aStream space; nextPutAll: s name; space; print: s allInstVarNames].
		aStream nextPut: $]  ].
	subs _ self subclasses.
	self == Class ifTrue:
		[aStream crtab: level+1; nextPutAll: '... all the Metaclasses ...'.
		subs _ subs reject: [:sub | sub isMeta]].
	"Print subclasses in alphabetical order"
	(subs asSortedCollection: [:x :y | x name < y name]) do:
		[:sub |
		sub printSubclassesOn: aStream callingSuperclass: self level: level + 1]

"""
    // 39 .. 164
    let expected = [16, 18, 224, 135, 16, 112, 210, 225, 135, 16, 136, 211, 135, 112, 213, 228, 135, 112, 214, 108, 20, 194, 118, 179, 172, 35, 16, 39, 225, 135, 20, 17, 232, 137, 118, 200, 164, 17, 109, 16, 136, 211, 135, 136, 21, 210, 225, 135, 136, 211, 135, 21, 217, 228, 125, 203, 135, 16, 42, 196, 135, 112, 219, 107, 112, 79, 198, 172, 22, 16, 136, 18, 118, 176, 224, 135, 44, 225, 135, 19, 137, 118, 200, 164, 4, 110, 22, 222, 125, 237, 107, 19, 137, 119, 200, 164, 9, 130, 72, 111, 23, 210, 24, 210, 178, 125, 131, 48, 137, 118, 200, 164, 10, 110, 22, 16, 112, 18, 118, 176, 131, 113, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveFromInheritanceTable() throws {
    let source = """
removeFromInheritanceTable: table
	"I have been deleted -- remove me from the given inheritance table"
	| keys list |
	"get keys first, since we may be deleting entries in the midst of the loop that follows"
	keys _ table keys.
	keys do:
		[:key | list _ (table at: key) copyWithout: self.
		list size = 0
			ifTrue: [table removeKey: key]
			ifFalse: [table at: key put: list]]

"""
    // 9 .. 41
    let expected = [16, 208, 105, 17, 137, 118, 200, 164, 21, 107, 16, 19, 192, 112, 225, 106, 18, 194, 117, 182, 155, 16, 19, 226, 147, 16, 19, 18, 193, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testUnmovedVarsFrom() throws {
    let source = """
unmovedVarsFrom: sup
	"Answer with an Array with true for fields with the same offset in this class as in super"
	| allInstVarNames supNames |
	allInstVarNames _ self allInstVarNames.
	supNames _ sup allInstVarNames.
	^ ((1 to: sup instSize) collect: [:i | (supNames at: i) = (allInstVarNames at: i)])

"""
    // 11 .. 36
    let expected = [112, 208, 105, 16, 208, 106, 118, 16, 211, 226, 137, 118, 200, 164, 9, 107, 18, 19, 192, 17, 19, 192, 182, 125, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testUpdateInheritanceTableOldSelf() throws {
    let source = """
updateInheritanceTable: table oldSelf: oldSelf
	"I have replaced an old behavior or class.  Update the given multiple inheritance table"
	table do:
		[:array | 1 to: array size do:
			[:i | (array at: i)==oldSelf ifTrue: [array at: i put: self]]]

"""
    // 5 .. 38
    let expected = [16, 137, 118, 200, 164, 25, 106, 118, 18, 194, 137, 118, 200, 164, 14, 107, 18, 19, 192, 17, 198, 156, 18, 19, 112, 193, 144, 115, 125, 240, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCompileConflictCodeFor() throws {
    let source = """
compileConflictCodeFor: selector
	| classes |
	classes _ SelectorsOfConflictMethods at: selector ifAbsent: [Array new].
	(classes includes: self name)  "This class already has conflict code for this selector"
		ifTrue: [^self].
	self compile: (self conflictCodeFor: selector)
		classified: 'conflicting inherited methods'
		notifying: nil.
	self insertClass: self selector: selector in: SelectorsOfConflictMethods

"""
    // 21 .. 55
    let expected = [65, 16, 137, 117, 200, 164, 3, 66, 204, 125, 240, 105, 17, 112, 212, 227, 152, 120, 112, 112, 16, 230, 39, 115, 131, 101, 135, 112, 112, 16, 65, 131, 104, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAccumulateInstVarNamesTraversedClasses() throws {
    let source = """
accumulateInstVarNames: names traversedClasses: classSet
	"accumulate instance variable names in 'names'.  Do this in depth-first,
	  left-to-right order.  This will give the ordering of instance variable names
	  expected by the compiler and other parts of the system."
	self superclasses do:
		[:each | each accumulateInstVarNames: names traversedClasses: classSet].
	(classSet includes: self) ifFalse:
		[names addAll: self instVarNames.
		classSet add: self]

"""
    // 15 .. 44
    let expected = [112, 208, 137, 118, 200, 164, 6, 106, 18, 16, 17, 241, 125, 203, 135, 17, 112, 229, 168, 9, 16, 112, 211, 226, 135, 17, 112, 228, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveClassSelectorIn() throws {
    let source = """
removeClass: aClass selector: selector in: aDictionary
	| list |
	list _ (aDictionary at: selector) copyWithout: aClass.
	list size = 0
		ifTrue: [aDictionary removeKey: selector]
		ifFalse: [aDictionary at: selector put: list]

"""
    // 7 .. 27
    let expected = [18, 17, 192, 16, 224, 107, 19, 194, 117, 182, 155, 18, 17, 225, 147, 18, 17, 19, 193, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsWords() throws {
    let source = """
isWords
	"Answer whether the receiver has 16-bit instance variables."

	^self isBytes not

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }


  func testCompilerClass() throws {
    let source = """
compilerClass
	"Return a compiler class appropriate for source methods of this class."

	^Compiler

"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }


  func testCopy() throws {
    let source = """
copy
	"Make a copy of the receiver without a list of subclasses."

	| myCopy savedSubclasses |
	savedSubclasses _ subclasses.
	subclasses _ nil.
	myCopy _ self shallowCopy.
	subclasses _ savedSubclasses.
	^myCopy methodDictionary: methodDict copy

"""
    // 9 .. 22
    let expected = [3, 105, 115, 99, 112, 208, 104, 17, 99, 16, 1, 210, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddSelectorWithMethod() throws {
    let source = """
addSelector: selector withMethod: compiledMethod
	"Add the message selector with the corresponding compiled method to the receiver's
	method dictionary."
	| wasThere |
	wasThere _ methodDict includesKey: selector.
	methodDict at: selector put: compiledMethod.
	self flushCache.
	"if the selector is indexed in SelectorsOfConflictMethods or SelectorsOfCopiedMethods,
	 remove it"
	((SelectorsOfConflictMethods at: selector ifAbsent: [Array new]) includes: self)
		ifTrue: [self removeClass: self selector: selector in: SelectorsOfConflictMethods].
	((SelectorsOfCopiedMethods at: selector ifAbsent: [Array new]) includes: self)
		ifTrue: [self removeClass: self selector: selector in: SelectorsOfCopiedMethods].
	wasThere
		ifTrue: [self checkChangeSelector: selector]
		ifFalse: [self subclasses do:
			[:sub | sub checkSuperAddSelector: selector]]

"""
    // 25 .. 100
    let expected = [1, 16, 224, 106, 1, 16, 17, 193, 135, 112, 209, 135, 67, 16, 137, 117, 200, 164, 3, 70, 204, 125, 245, 112, 228, 158, 112, 112, 16, 67, 131, 98, 135, 71, 16, 137, 117, 200, 164, 3, 70, 204, 125, 245, 112, 228, 158, 112, 112, 16, 71, 131, 98, 135, 18, 156, 112, 16, 234, 164, 13, 112, 216, 137, 118, 200, 164, 5, 107, 19, 16, 233, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveSelectorSimply() throws {
    let source = """
removeSelectorSimply: selector
	"Remove the message selector from the receiver's method dictionary.
	Internal access from compiler."

	methodDict removeKey: selector ifAbsent: [^self].
	self flushCache

"""
    // 7 .. 20
    let expected = [1, 16, 137, 117, 200, 164, 1, 120, 240, 135, 112, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCompiledMethodAt() throws {
    let source = """
compiledMethodAt: selector
	"Answer the compiled method associated with the message selector in the
	receiver's method dictionary.  If the selector is not in the dictionary,
	create an error notification."

	^methodDict at: selector

"""
    // 3 .. 6
    let expected = [1, 16, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testScopeHasIfTrue() throws {
    let source = """
scopeHas: varName ifTrue: assocBlock
	"Look up varName in this class, its superclasses, and Smalltalk.  If it is there,
	pass the association to assocBlock, and answer true; else answer false."
	| assoc |
	self withAllSuperclasses do:
		[:sup |
		(sup poolHas: varName ifTrue: assocBlock) ifTrue: [^true]].
	assoc _ Smalltalk associationAt: varName ifAbsent: [].
	assoc == nil
		ifFalse:
			[assocBlock value: assoc.
			^true].
	^false

"""
    // 11 .. 50
    let expected = [112, 208, 137, 118, 200, 164, 9, 107, 19, 16, 17, 241, 152, 121, 115, 125, 203, 135, 67, 16, 137, 117, 200, 164, 2, 115, 125, 242, 106, 18, 115, 198, 168, 5, 17, 18, 202, 135, 121, 122]
    try runningSource(source, expecting: expected)
  }

  func testWithAllSuperclasses() throws {
    let source = """
withAllSuperclasses
	"Answer an OrderedCollection of superclasses including this class in breadth first order."
	| subs |
	subs _ self allSuperclasses.
	subs addFirst: self.
	^subs

"""
    // 7 .. 15
    let expected = [112, 208, 104, 16, 112, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstVarNames() throws {
    let source = """
instVarNames
	"Answer an Array of the instance variable names.  Behaviors must make up fake
	local instance variable names because Behaviors have instance variables for the
	purpose of compiling methods, but these are not named instance variables.  "

	| mySize superSize |
	mySize _ self instSize.
	superSize _
		superclass == nil
			ifTrue: [0]
			ifFalse: [superclass instSize].
	mySize = superSize ifTrue: [^#()].
	^(superSize + 1 to: mySize) collect: [:i | 'inst' , i printString]

"""
    // 17 .. 52
    let expected = [112, 208, 104, 0, 115, 198, 153, 117, 145, 0, 208, 105, 16, 17, 182, 153, 33, 124, 17, 118, 176, 16, 227, 137, 118, 200, 164, 6, 106, 37, 18, 214, 228, 125, 226, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllInstVarNames() throws {
    let source = """
allInstVarNames
	"Answer an Array of the names of the receiver's instance variables."
	| names |
	names _ OrderedCollection new.
	self accumulateInstVarNames: names traversedClasses: Set new.
	^names

"""
    // 9 .. 19
    let expected = [64, 204, 104, 112, 16, 66, 204, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIncludesSelector() throws {
    let source = """
includesSelector: aSymbol
	"Answer whether the message whose selector is the argument is in the
	method dictionary of the receiver's class."

	^methodDict includesKey: aSymbol

"""
    // 5 .. 8
    let expected = [1, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testClassVarNames() throws {
    let source = """
classVarNames
	"Answer a Set of the receiver's class variable names.  Since the receiver does
	not retain knowledge of class variables, the method fakes it by creating an empty set."

	^Set new

"""
    // 5 .. 7
    let expected = [64, 204, 124]
    try runningSource(source, expecting: expected)
  }

  func testSharedPools() throws {
    let source = """
sharedPools
	"Answer a Set of the pools, dictionaries, that the receiver shares.  Since the receiver
	does not retain knowledge of pool dictionaries, the method fakes it by creating an
	empty array.  Subclasses, such as class Class, override this message."

	^Set new

"""
    // 5 .. 7
    let expected = [64, 204, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllVarNamesSelect() throws {
    let source = """
allVarNamesSelect: selectBlock
	"Answer a collection of all the static variable names defined for the receiver which satisfy the condition in selectBlock.  Test class and pool variables, including superclass variables.  Also include global variables."

	| set |
	set _ self classPool keys select: selectBlock.
	self sharedPools do: [:pool | set addAll: (pool keys select: selectBlock)].
	superclass == nil
		ifTrue:	[set addAll: (Smalltalk keys select: selectBlock)]
		ifFalse: [set addAll: (superclass allVarNamesSelect: selectBlock)].
	^set

"""
    // 17 .. 58
    let expected = [112, 210, 209, 16, 224, 105, 112, 211, 137, 118, 200, 164, 8, 106, 17, 18, 209, 16, 224, 228, 125, 203, 135, 0, 115, 198, 158, 17, 70, 209, 16, 224, 228, 148, 17, 0, 16, 229, 228, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testEvaluatorClass() throws {
    let source = """
evaluatorClass
	"Return an evaluator class appropriate for evaluating expressions in the
	context of instances of this class."

	^Compiler

"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testBasicNew() throws {
    let source = """
basicNew
	"Answer a new instance of the receiver (which is a class) with no indexable
	variables.  Fail if the class is indexable.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 70>
	self isVariable ifTrue: [^self basicNew: 0].
	self primitiveFailed

"""
    // 13 .. 23
    let expected = [112, 209, 155, 112, 117, 224, 124, 112, 210, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testWithAllSubclasses() throws {
    let source = """
withAllSubclasses
	"Answer an OrderedCollection of subclasses including this class in breadth first order."
	| subs |
	subs _ self allSubclasses.
	subs addFirst: self.
	^subs

"""
    // 7 .. 15
    let expected = [112, 208, 104, 16, 112, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testParserClass() throws {
    let source = """
parserClass
	"Return a parser class to use for parsing methods in this class."

	^self compilerClass preferredParserClass

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
