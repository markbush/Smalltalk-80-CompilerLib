import XCTest
@testable import CompilerLib

final class CompileClassTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Class", instanceVariables: ["superclass", "methodDict", "format", "subclasses", "instanceVariables", "organization", "name", "classPool", "sharedPools"])
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

  func testRemoveFromSystem() throws {
    let source = """
removeFromSystem
	"Forget the receiver, and all of its subclasses, from the Smalltalk global dictionary.  Any existing instances will refer to an obsolete version of the receiver."

	Smalltalk removeClassNamed: self name. 			"remove from system dictionary"

"""
    // 9 .. 14
    let expected = [65, 112, 210, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAllClassVarNames() throws {
    let source = """
allClassVarNames
	"Answer a Set of the names of the receiver's class variables, including those
	defined in the superclasses of the receiver."
	| aSet |
	superclass == nil
		ifTrue:
			[^self classVarNames]  "This is the keys so it is a new Set."
		ifFalse:
			[aSet _ superclass allClassVarNames.
			aSet addAll: self classVarNames.
			^aSet]

"""
    // 9 .. 25
    let expected = [0, 115, 198, 154, 112, 210, 124, 0, 208, 104, 16, 112, 210, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubclassInstanceVariableNamesClassVariableNamesPoolDictionariesCategory() throws {
    let source = """
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a subclass
	of an existing class (the receiver)."

	self isVariable
		ifTrue:
			[self isPointers
				ifTrue: [^self
							variableSubclass: t
							instanceVariableNames: f
							classVariableNames: d
							poolDictionaries: s
							category: cat].
			self isBytes
				ifTrue: [^self
							variableByteSubclass: t
							instanceVariableNames: f
							classVariableNames: d
							poolDictionaries: s
							category: cat].
			^self
				variableWordSubclass: t
				instanceVariableNames: f
				classVariableNames: d
				poolDictionaries: s
				category: cat].
	^self class
		name: t
		inEnvironment: Smalltalk
		subclassOf: self
		instanceVariableNames: f
		variable: false
		words: true
		pointers: true
		classVariableNames: d
		poolDictionaries: s
		category: cat
		comment: nil
		changed: false

"""
    // 23 .. 79
    let expected = [112, 213, 172, 35, 112, 209, 172, 9, 112, 16, 17, 18, 19, 20, 131, 160, 124, 112, 211, 172, 9, 112, 16, 17, 18, 19, 20, 131, 162, 124, 112, 16, 17, 18, 19, 20, 131, 164, 124, 112, 199, 16, 71, 112, 17, 114, 113, 113, 18, 19, 20, 115, 114, 132, 12, 6, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllSharedPools() throws {
    let source = """
allSharedPools
	"Answer a Set of the pools the receiver shares, including those defined
	in the superclasses of the receiver."

	| aSet |
	superclass == nil
		ifTrue:
			[^self sharedPools copy]
		ifFalse:
			[aSet _ superclass allSharedPools.
			aSet addAll: self sharedPools.
			^aSet]

"""
    // 11 .. 28
    let expected = [0, 115, 198, 155, 112, 210, 211, 124, 0, 208, 104, 16, 112, 210, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testValidateFromInInstanceVariableNamesMethods() throws {
    let source = """
validateFrom: oldClass in: environ instanceVariableNames: invalidFields methods: invalidMethods
	"Recompile the receiver and redefine its subclasses if necessary."

	super
		validateFrom: oldClass
		in: environ
		instanceVariableNames: invalidFields
		methods: invalidMethods.
	self ~~ oldClass
		ifTrue:
			[environ at: name put: self.
			self updateInheritanceTables: oldClass.
			oldClass obsolete]

"""
    // 13 .. 38
    let expected = [112, 16, 17, 18, 19, 133, 128, 135, 112, 16, 227, 172, 12, 17, 6, 112, 193, 135, 112, 16, 225, 135, 16, 210, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOut() throws {
    let source = """
fileOut
	"Create a file whose name is the name of the receiver with -.st- as the
	extension, and file a description of the receiver onto it"

	| fileStream |
	Transcript refresh; cr; cr; show: 'Filing out class:'.
					fileStream _ Disk file: self name , '.st'.
	fileStream timeStamp.
	self fileOutOn: fileStream
		moveSource: false
		toFile: 0.
	fileStream shorten; close.
	self removeFromChanges.

"""
    // 33 .. 72
    let expected = [64, 136, 209, 135, 136, 210, 135, 136, 210, 135, 36, 227, 135, 70, 112, 216, 41, 231, 229, 104, 16, 218, 135, 112, 16, 114, 117, 131, 107, 135, 16, 136, 220, 135, 221, 135, 112, 222, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testVariableByteSubclassInstanceVariableNamesClassVariableNamesPoolDictionariesCategory() throws {
    let source = """
variableByteSubclass: t instanceVariableNames: f
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a subclass
	of an existing class (the receiver) in which the subclass is to have indexable
	byte-sized nonpointer variables."

	self instSize > 0
		ifTrue: [^self error: 'cannot make a byte subclass of a class with named fields'].
	(self isVariable and: [self isWords])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with word fields'].
	(self isVariable and: [self isPointers])
		ifTrue: [^self error:
					'cannot make a byte subclass of a class with pointer fields'].
	^self class name: t
		inEnvironment: Smalltalk
		subclassOf: self
		instanceVariableNames: f
		variable: true
		words: false
		pointers: false
		classVariableNames: d
		poolDictionaries: s
		category: cat
		comment: nil
		changed: false

"""
    // 27 .. 77
    let expected = [112, 210, 117, 179, 155, 112, 33, 224, 124, 112, 213, 154, 112, 212, 144, 114, 155, 112, 35, 224, 124, 112, 213, 154, 112, 215, 144, 114, 155, 112, 38, 224, 124, 112, 199, 16, 73, 112, 17, 113, 114, 114, 18, 19, 20, 115, 114, 132, 12, 8, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddInstVarName() throws {
    let source = """
addInstVarName: aString
	"Add the argument, aString, as one of the receiver's instance variables."
	superclass class
		name: self name
		inEnvironment: Smalltalk
		subclassOf: superclass
		instanceVariableNames: self instanceVariablesString , aString
		variable: self isVariable
		words: self isWords
		pointers: self isPointers
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category
		comment: nil
		changed: false

"""
    // 25 .. 53
    let expected = [0, 199, 112, 209, 66, 0, 112, 212, 16, 227, 112, 213, 112, 214, 112, 215, 112, 216, 112, 217, 112, 218, 115, 114, 132, 12, 0, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHasMethods() throws {
    let source = """
hasMethods
	"Answer a Boolean as to whether any methods are defined for the receiver
	(includes whether there are methods defined in the receiver's metaclass)."

	^super hasMethods or: [self class hasMethods]

"""
    // 7 .. 16
    let expected = [112, 133, 0, 153, 113, 146, 112, 199, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveInstVarName() throws {
    let source = """
removeInstVarName: aString
	"Remove the argument, aString, as one of the receiver's instance variables."
	| newInstVarString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newInstVarString _ ''.
	(self instVarNames copyWithout: aString) do:
		[:varName | newInstVarString _ newInstVarString , ' ' , varName].
	superclass class
		name: self name
		inEnvironment: Smalltalk
		subclassOf: superclass
		instanceVariableNames: newInstVarString
		variable: self isVariable
		words: self isWords
		pointers: self isPointers
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category
		comment: nil
		changed: false

"""
    // 37 .. 97
    let expected = [112, 212, 16, 227, 168, 6, 112, 16, 34, 225, 224, 135, 37, 105, 112, 212, 16, 230, 137, 118, 200, 164, 9, 106, 17, 39, 225, 18, 225, 129, 65, 125, 203, 135, 0, 199, 112, 217, 74, 0, 17, 112, 219, 112, 220, 112, 221, 112, 222, 112, 223, 112, 131, 16, 115, 114, 132, 12, 8, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testVariableSubclassInstanceVariableNamesClassVariableNamesPoolDictionariesCategory() throws {
    let source = """
variableSubclass: t instanceVariableNames: f
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a subclass
	of an existing class (the receiver) in which the subclass is to have indexable
	pointer variables."

	self isBits
		ifTrue:
			[^self error:
				'cannot make a pointer subclass of a class with non-pointer fields'].
	^self class name: t
		inEnvironment: Smalltalk
		subclassOf: self
		instanceVariableNames: f
		variable: true
		words: true
		pointers: true
		classVariableNames: d
		poolDictionaries: s
		category: cat
		comment: nil
		changed: false

"""
    // 17 .. 41
    let expected = [112, 210, 155, 112, 33, 224, 124, 112, 199, 16, 68, 112, 17, 113, 113, 113, 18, 19, 20, 115, 114, 132, 12, 3, 124]
    try runningSource(source, expecting: expected)
  }

  func testVariableWordSubclassInstanceVariableNamesClassVariableNamesPoolDictionariesCategory() throws {
    let source = """
variableWordSubclass: t instanceVariableNames: f
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a subclass
	of an existing class (the receiver) in which the subclass is to have indexable
	word-sized nonpointer variables."

	self instSize > 0
		ifTrue: [^self error:
					'cannot make a word subclass of a class with named fields'].
	self isBytes
		ifTrue: [^self error: 'cannot make a word subclass of a class with byte fields'].
	(self isVariable and: [self isPointers])
		ifTrue: [^self error:
					'cannot make a word subclass of a class with pointer fields'].
	^self class name: t
		inEnvironment: Smalltalk
		subclassOf: self
		instanceVariableNames: f
		variable: true
		words: true
		pointers: false
		classVariableNames: d
		poolDictionaries: s
		category: cat
		comment: nil
		changed: false

"""
    // 27 .. 72
    let expected = [112, 210, 117, 179, 155, 112, 33, 224, 124, 112, 212, 155, 112, 35, 224, 124, 112, 215, 154, 112, 214, 144, 114, 155, 112, 37, 224, 124, 112, 199, 16, 73, 112, 17, 113, 113, 114, 18, 19, 20, 115, 114, 132, 12, 8, 124]
    try runningSource(source, expecting: expected)
  }

  func testSuperclassMethodDictFormatNameOrganizationInstVarNamesClassPoolSharedPools() throws {
    let source = """
superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet
	"Answer an instance of me, a new class, using the arguments of the message
	as the needed information."

	superclass _ sup.
	methodDict _ md.
	format _ ft.
	name _ nm.
	organization _ org.
	instanceVariables _ nilOrArray.
	classPool _ pool.
	sharedPools _ poolSet

"""
    // 7 .. 24
    let expected = [16, 96, 17, 97, 18, 98, 19, 102, 20, 101, 21, 100, 22, 103, 23, 130, 8, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyForValidation() throws {
    let source = """
copyForValidation
	"Make a copy of the receiver (a class) but do not install the created class
	as a new class in the system.  This is used for creating a new version of
	the receiver in which the installation is deferred until all changes are
	successfully completed."

	^self class copy new
		superclass: superclass
		methodDict: methodDict copy
		format: format
		name: name
		organization: organization
		instVarNames: instanceVariables copy
		classPool: classPool
		sharedPools: sharedPools

"""
    // 7 .. 24
    let expected = [112, 199, 209, 204, 0, 1, 209, 2, 6, 5, 4, 209, 7, 8, 132, 8, 0, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveSharedPool() throws {
    let source = """
removeSharedPool: aDictionary
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool dictionaries.
	Create an error if the dictionary is not one of the pools."

	| satisfiedSet workingSet aSubclass|
	(self sharedPools includes: aDictionary)
		ifFalse: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self allSuperclasses select: [:class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [sharedPools remove: aDictionary.
				sharedPools isEmpty ifTrue: [sharedPools _ nil].
				^self].

	"second get all the subclasses that reference aDictionary through me rather than a
	superclass that is one of my subclasses."

	workingSet _ self subclasses asOrderedCollection.
	satisfiedSet _ Set new.
	[workingSet isEmpty] whileFalse:
		[aSubclass _ workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse:
				[satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do:
		[:aSubclass |
		aDictionary associationsDo:
			[:aGlobal |
			(aSubclass whichSelectorsReferTo: aGlobal) isEmpty
				ifFalse: [^self error: aGlobal key
								, ' is still used in code of class '
								, aSubclass name]]].
	sharedPools remove: aDictionary.
	sharedPools isEmpty ifTrue: [sharedPools _ nil]

"""
    // 43 .. 166
    let expected = [112, 211, 16, 226, 168, 4, 112, 33, 224, 124, 112, 215, 137, 118, 200, 164, 6, 108, 20, 211, 16, 226, 125, 230, 213, 168, 11, 8, 16, 228, 135, 8, 213, 154, 115, 130, 8, 120, 112, 217, 216, 106, 74, 204, 105, 18, 213, 168, 20, 18, 219, 107, 19, 211, 16, 226, 168, 9, 17, 19, 236, 135, 18, 19, 217, 237, 135, 163, 232, 17, 112, 236, 135, 17, 137, 118, 200, 164, 32, 107, 16, 137, 118, 200, 164, 23, 109, 19, 21, 131, 51, 213, 154, 115, 164, 12, 112, 21, 131, 16, 49, 239, 19, 131, 18, 239, 224, 124, 125, 238, 125, 203, 135, 8, 16, 228, 135, 8, 213, 154, 115, 130, 8, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsObsolete() throws {
    let source = """
isObsolete
	"Answer whether the receiver is an obsolete class."

	^self class isObsolete 		"ask the metaclass"

"""
    // 5 .. 8
    let expected = [112, 199, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testName() throws {
    let source = """
name
	"Answer the name of the receiver."

	name == nil
		ifTrue: [^super name]
		ifFalse: [^name]

"""
    // 7 .. 16
    let expected = [6, 115, 198, 155, 112, 133, 0, 124, 6, 124]
    try runningSource(source, expecting: expected)
  }

  func testClassVarNames() throws {
    let source = """
classVarNames
	"Answer a Set of the names of the class variables defined in the receiver."

	^self classPool keys

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testDeclare() throws {
    let source = """
declare: varString
	"Declare class variables common to all instances.  Answer whether
	recompilation is advisable."

	| newVars conflicts assoc class |
	newVars _
		(Scanner new scanFieldNames: varString)
			collect: [:x | x asSymbol].
	newVars do:
		[:var | var first isLowercase
			ifTrue: [self error: var, ' class variable name should be capitalized; proceed to include anyway.']].
	conflicts _ false.
	classPool == nil
		ifFalse: [(classPool keys reject: [:x | newVars includes: x]) do:
					[:var | self removeClassVarName: var]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self scopeHas: var ifTrue: [:ignored | ignored])
				ifTrue:
					[self error: var , ' is defined elsewhere'.
					conflicts _ true]].
	newVars size > 0
		ifTrue:
			[classPool _ self classPool.
			"in case it was nil"
			newVars do: [:var | classPool declare: var from: Undeclared]].
	^conflicts

"""
    // 41 .. 181
    let expected = [66, 204, 16, 225, 137, 118, 200, 164, 4, 109, 21, 211, 125, 224, 105, 17, 137, 118, 200, 164, 13, 110, 22, 216, 215, 157, 112, 22, 38, 229, 228, 144, 115, 125, 203, 135, 114, 106, 7, 115, 198, 168, 25, 7, 218, 137, 118, 200, 164, 5, 109, 17, 21, 235, 125, 233, 137, 118, 200, 164, 5, 110, 112, 22, 236, 125, 203, 135, 17, 137, 118, 200, 164, 6, 110, 112, 222, 22, 237, 125, 233, 137, 118, 200, 164, 27, 110, 112, 22, 137, 118, 200, 164, 3, 111, 23, 125, 131, 80, 172, 10, 112, 22, 47, 229, 228, 135, 113, 129, 66, 144, 115, 125, 203, 135, 17, 194, 117, 179, 172, 18, 112, 222, 103, 17, 137, 118, 200, 164, 7, 110, 7, 22, 82, 131, 81, 125, 203, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testSharing() throws {
    let source = """
sharing: poolString
	"Set up sharedPools.  Answer whether recompilation is advisable."

	| oldPools poolName pool |
	oldPools _ self sharedPools.
	sharedPools _ Set new.
	(Scanner new scanFieldNames: poolString) do:
		[:poolName |
		sharedPools add: (Smalltalk at: poolName asSymbol)].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	oldPools
		detect: [:pool | (self sharedPools includes: pool) not]
		ifNone: [^false].
	^true	"A pool got deleted - who knows if there are still references?"

"""
    // 25 .. 78
    let expected = [112, 208, 105, 65, 204, 130, 8, 67, 204, 16, 226, 137, 118, 200, 164, 8, 106, 8, 69, 18, 214, 192, 228, 125, 203, 135, 8, 215, 154, 115, 130, 8, 17, 137, 118, 200, 164, 7, 107, 112, 208, 19, 234, 217, 125, 137, 117, 200, 164, 1, 122, 248, 135, 121]
    try runningSource(source, expecting: expected)
  }

  func testRename() throws {
    let source = """
rename: aString
	"The new name of the receiver is the argument, aString."

	| newName |
	newName _ aString asSymbol.
	(Smalltalk includesKey: newName)
		ifTrue: [^self error: newName , ' already exists'].
	Smalltalk renameClass: self as: newName.
	name _ newName.
	self comment: self comment.
	self class comment: self class comment

"""
    // 21 .. 53
    let expected = [16, 208, 105, 69, 17, 228, 157, 112, 17, 35, 226, 225, 124, 69, 112, 17, 246, 135, 17, 102, 112, 112, 216, 231, 135, 112, 199, 112, 199, 216, 231, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveClassVarName() throws {
    let source = """
removeClassVarName: aString
	"Remove the class variable whose name is the argument, aString, from the names
	defined in the receiver, a class."

	| anAssoc aSymbol |
	aSymbol _ aString asSymbol.
	(classPool includesKey: aSymbol)
		ifFalse: [^self error: aString, ' is not a class variable'].
	anAssoc _ classPool associationAt: aSymbol.
	self withAllSubclasses do:
		[:subclass |
		(Array with: subclass with: subclass class) do:
			[:classOrMeta |
			(classOrMeta whichSelectorsReferTo: (classPool associationAt: aSymbol))
				isEmpty
					ifFalse: [^self error: aString
								, ' is still used in code of class '
								, classOrMeta name]]].
	classPool removeKey: aSymbol

"""
    // 31 .. 96
    let expected = [16, 208, 106, 7, 18, 228, 168, 6, 112, 16, 35, 226, 225, 124, 7, 18, 229, 105, 112, 214, 137, 118, 200, 164, 34, 107, 72, 19, 19, 199, 247, 137, 118, 200, 164, 21, 108, 20, 7, 18, 229, 236, 219, 154, 115, 164, 9, 112, 16, 41, 226, 20, 218, 226, 225, 124, 125, 203, 125, 203, 135, 7, 18, 237, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddSharedPool() throws {
    let source = """
addSharedPool: aDictionary
	"Add the argument, aDictionary, as one of the receiver's pool dictionaries.  Create
	an error if the dictionary is already one of the pools."

	(self sharedPools includes: aDictionary)
		ifTrue: [^self error: 'The dictionary is already in my pool'].
	sharedPools == nil
		ifTrue: [sharedPools _ Set with: aDictionary]
		ifFalse: [sharedPools add: aDictionary]

"""
    // 17 .. 40
    let expected = [112, 211, 16, 226, 155, 112, 33, 224, 124, 8, 115, 198, 157, 70, 16, 229, 129, 8, 146, 8, 16, 228, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSharedPools() throws {
    let source = """
sharedPools
	"Answer a Set of the pool dictionaries declared in the receiver."

	sharedPools == nil
		ifTrue: [^Set new]
		ifFalse: [^sharedPools]

"""
    // 5 .. 13
    let expected = [8, 115, 198, 154, 64, 204, 124, 8, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOut() throws {
    let source = """
printOut
	"Create a readable version of my definition, and send to a printer.
	Defaults to fileOut."
	self fileOut

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testClassPool() throws {
    let source = """
classPool
	"Answer the dictionary of class variables."

	classPool == nil
		ifTrue: [^Dictionary new]
		ifFalse: [^classPool]

"""
    // 5 .. 13
    let expected = [7, 115, 198, 154, 64, 204, 124, 7, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddClassVarName() throws {
    let source = """
addClassVarName: aString
	"Add the argument, aString, as a class variable of the receiver."

	aString first isLowercase
		ifTrue: [^self error: aString, ' class variable name should be capitalized; proceed to include anyway.'].
	self withAllSubclasses do:
		[:subclass |
		subclass
			poolHas: aString asSymbol
			ifTrue:
				[:ignored |
				^self error: aString , ' is already used as a variable name in ' , subclass name]].
	classPool _ self classPool.  "might be nil"
	classPool add: (Association key: aString asSymbol value: nil)

"""
    // 31 .. 82
    let expected = [16, 212, 211, 157, 112, 16, 34, 225, 224, 124, 112, 213, 137, 118, 200, 164, 21, 105, 17, 16, 215, 137, 118, 200, 164, 10, 106, 112, 16, 40, 225, 17, 217, 225, 224, 124, 246, 125, 203, 135, 112, 218, 103, 7, 77, 16, 215, 115, 252, 235, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	| newClass |
	newClass _ self class copy new
		superclass: superclass
		methodDict: methodDict copy
		format: format
		name: name
		organization: organization copy
		instVarNames: instanceVariables copy
		classPool: classPool copy
		sharedPools: sharedPools.
	Class instSize to: self class instSize do:
		[:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
	^newClass

"""
    // 17 .. 58
    let expected = [112, 199, 209, 204, 0, 1, 209, 2, 6, 5, 209, 4, 209, 7, 209, 8, 132, 8, 0, 104, 68, 211, 112, 199, 211, 137, 118, 200, 164, 8, 105, 16, 17, 112, 17, 230, 245, 125, 242, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

// TODO: Puts fileOutOn:moveSource:toFile: in literals twice!
//   func testFileOutOnMoveSourceToFile() throws {
//     let source = """
// fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
// 	"File a description of the receiver on aFileStream.  If the boolean argument,
// 	moveSource, is true, then set the trailing bytes to the position of aFileStream and
// 	to fileIndex in order to indicate where to find the source code."
//
// 	Transcript cr; show: name.
// 	super
// 		fileOutOn: aFileStream
// 		moveSource: moveSource
// 		toFile: fileIndex.
// 	self class nonTrivial
// 		ifTrue:
// 			[aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; cr; cr.
// 			self class
// 				fileOutOn: aFileStream
// 				moveSource: moveSource
// 				toFile: fileIndex].
//
// """
//     // 21 .. 61
//     let expected = [64, 136, 209, 135, 6, 226, 135, 112, 16, 17, 18, 133, 99, 135, 112, 199, 215, 172, 21, 16, 136, 209, 135, 136, 37, 228, 135, 136, 209, 135, 209, 135, 112, 199, 16, 17, 18, 131, 102, 135, 120]
//     try runningSource(source, expecting: expected)
//   }

  func testRemoveFromChanges() throws {
    let source = """
removeFromChanges
	"References to the receiver, a class, and its metaclass should no longer be included
	in the system ChangeSet."
	Smalltalk changes removeClassChanges: self

"""
    // 9 .. 14
    let expected = [66, 209, 112, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testObsolete() throws {
    let source = """
obsolete
	"Change the receiver to an obsolete class by changing its name to have the prefix -AnObsolete-, and nilling the fields of any instances."

	self isPointers ifTrue:
		[self allInstancesDo: [:instance | instance nilFields]]. 	"nil fields of instances"
	('AnObsolete*' match: name) ifFalse:
		[name _ 'AnObsolete' , name].
	classPool _ Dictionary new.
	self class obsolete.
	super obsolete

"""
    // 23 .. 59
    let expected = [112, 210, 172, 12, 112, 137, 118, 200, 164, 4, 104, 16, 209, 125, 224, 135, 38, 6, 229, 168, 4, 36, 6, 227, 102, 71, 204, 103, 112, 199, 216, 135, 112, 133, 8, 135, 120]
    try runningSource(source, expecting: expected)
  }

// TODO: Adds extra item to literals
//   func testCompileAllFrom() throws {
//     let source = """
// compileAllFrom: otherClass
// 	super compileAllFrom: otherClass.
// 	self class compileAllFrom: otherClass class
//
// """
//     // 9 .. 20
//     let expected = [112, 16, 133, 32, 135, 112, 199, 16, 199, 225, 135, 120]
//     try runningSource(source, expecting: expected)
//   }

  func testHasMultipleSuperclasses() throws {
    let source = """
hasMultipleSuperclasses
	^self class instHasMultipleSuperclasses

"""
    // 5 .. 8
    let expected = [112, 199, 208, 124]
    try runningSource(source, expecting: expected)
  }


  func testPoolHasIfTrue() throws {
    let source = """
poolHas: varName ifTrue: assocBlock
	"Look up the first argument in the context of the receiver.  If it is there,
	pass the association to assocBlock, and answer true, else answer false."
	| assoc pool |
	assoc _ self classPool associationAt: varName ifAbsent: [].
	assoc == nil
		ifFalse:
			[assocBlock value: assoc.
			^true].
	self sharedPools do:
		[:pool |
		assoc _ pool associationAt: varName ifAbsent: [].
		assoc == nil
			ifFalse:
				[assocBlock value: assoc.
				^true]].
	^ false

"""
    // 9 .. 64
    let expected = [112, 209, 16, 137, 117, 200, 164, 2, 115, 125, 240, 106, 18, 115, 198, 168, 5, 17, 18, 202, 135, 121, 112, 210, 137, 118, 200, 164, 24, 107, 19, 16, 137, 117, 200, 164, 2, 115, 125, 240, 106, 18, 115, 198, 153, 115, 148, 17, 18, 202, 135, 121, 125, 203, 135, 122]
    try runningSource(source, expecting: expected)
  }

  func testSubclassOtherSupersInstanceVariableNamesClassVariableNamesCategory() throws {
    let source = """
subclass: t otherSupers: others instanceVariableNames: f classVariableNames: d category: cat
	"This is the standard initialization message for creating a new class as a subclass
	of an existing class (the receiver)."

	self isVariable
		ifTrue:
			[self isPointers
				ifTrue: [^self
							variableSubclass: t
							instanceVariableNames: f
							classVariableNames: d
							poolDictionaries: ''
							category: cat].
			self isBytes
				ifTrue: [^self
							variableByteSubclass: t
							instanceVariableNames: f
							classVariableNames: d
							poolDictionaries: ''
							category: cat].
			^self
				variableWordSubclass: t
				instanceVariableNames: f
				classVariableNames: d
				poolDictionaries: ''
				category: cat].
	^self class
		name: t
		inEnvironment: Smalltalk
		subclassOf: self and: others
		instanceVariableNames: f
		variable: false
		words: true
		pointers: true
		classVariableNames: d
		poolDictionaries: ''
		category: cat
		comment: nil
		changed: false

"""
    // 25 .. 82
    let expected = [112, 214, 172, 35, 112, 210, 172, 9, 112, 16, 18, 19, 33, 20, 131, 160, 124, 112, 212, 172, 9, 112, 16, 18, 19, 33, 20, 131, 163, 124, 112, 16, 18, 19, 33, 20, 131, 165, 124, 112, 199, 16, 72, 112, 17, 18, 114, 113, 113, 19, 33, 20, 115, 114, 132, 13, 7, 124]
    try runningSource(source, expecting: expected)
  }

}
