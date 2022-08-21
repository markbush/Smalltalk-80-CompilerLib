import XCTest
@testable import CompilerLib

final class CompileMetaclassTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Metaclass", instanceVariables: ["superclass", "methodDict", "format", "subclasses", "instanceVariables", "organization", "thisClass"])
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

  func testSubclassOf() throws {
    let source = """
subclassOf: superMeta
	"Change the receiver to be a subclass of the argument, superMeta, a metaclass.
	Reset the receiver's method dictionary and properties."

	superclass _ superMeta.
	methodDict _ MethodDictionary new.
	format _ superMeta format.
	instanceVariables _ nil

"""
    // 7 .. 17
    let expected = [16, 96, 64, 204, 97, 16, 209, 98, 115, 100, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutOnMoveSourceToFile() throws {
    let source = """
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	"File me out on aFileStream."

	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex.
	(methodDict includesKey: #initialize)
		ifTrue:
			[aFileStream cr.
			aFileStream cr.
			aFileStream nextChunkPut: thisClass name , ' initialize'.
			aFileStream cr]

"""
    // 21 .. 49
    let expected = [112, 16, 17, 18, 133, 96, 135, 1, 39, 230, 172, 16, 16, 209, 135, 16, 209, 135, 16, 6, 212, 37, 227, 226, 135, 16, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	"Make a copy of the receiver without a list of subclasses.  Share the
	reference to the sole instance."

	| copy t |
	t _ thisClass.
	thisClass _ nil.
	copy _ super copy.
	thisClass _ t.
	^copy

"""
    // 7 .. 18
    let expected = [6, 105, 115, 102, 112, 133, 0, 104, 17, 102, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubclasses() throws {
    let source = """
subclasses
	"Answer the receiver's subclasses."
	| temp |
	self == Class class
		ifTrue: ["Meta-Object is exceptional subclass of Class"
				temp _ thisClass subclasses copy.
				temp remove: Object class.
				^temp collect: [:aSubClass | aSubClass class]].
	thisClass == nil
		ifTrue: [^Set new]
		ifFalse: [^thisClass subclasses collect: [:aSubClass | aSubClass class]]

"""
    // 17 .. 63
    let expected = [112, 69, 199, 198, 172, 21, 6, 209, 208, 104, 16, 67, 199, 226, 135, 16, 137, 118, 200, 164, 4, 105, 17, 199, 125, 228, 124, 6, 115, 198, 154, 70, 204, 124, 6, 209, 137, 118, 200, 164, 4, 105, 17, 199, 125, 228, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstHasMultipleSuperclasses() throws {
    let source = """
instHasMultipleSuperclasses
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testNew() throws {
    let source = """
new
	"The receiver can only have one instance.  Create it or complain that
	one already exists."

	thisClass == nil
		ifTrue: [^thisClass _ super new]
		ifFalse: [self error: 'A Metaclass should only have one instance!']

"""
    // 11 .. 25
    let expected = [6, 115, 198, 157, 112, 133, 2, 129, 6, 124, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNonTrivial() throws {
    let source = """
nonTrivial
	^self instVarNames size > 0 or: [methodDict size > 0 or: [self comment size > 0]]

"""
    // 7 .. 28
    let expected = [112, 209, 194, 117, 179, 154, 113, 164, 12, 1, 194, 117, 179, 153, 113, 148, 112, 208, 194, 117, 179, 124]
    try runningSource(source, expecting: expected)
  }

  func testNameInEnvironmentSubclassOfInstanceVariableNamesVariableWordsPointersClassVariableNamesPoolDictionariesCategoryCommentChanged() throws {
    let source = """
name: newName inEnvironment: environ subclassOf: sup instanceVariableNames: instVarString variable: v words: w pointers: p classVariableNames: classVarString poolDictionaries: poolString category: categoryName comment: commentString changed: changed
	"Create a new metaclass from the information provided in the arguments.
	Create an error if the name does not begin with an uppercase letter or if a
	class of the same name already exists."

	| wasPresent oldClass newClass invalidFields invalidMethods |
	newName first isUppercase
		ifFalse:
			[self error: 'Class names must be capitalized'.
			^false].
	(wasPresent _ environ includesKey: newName)
		ifTrue:
			[oldClass _ environ at: newName.
			(oldClass isKindOf: Behavior)
				ifFalse:
					[self error: newName , ' already exists!  Proceed will store over it'.
					wasPresent _ false.
					oldClass _ self newNamed: newName]]
		ifFalse: [oldClass _ self newNamed: newName].
	newClass _ oldClass copy.
	invalidFields _
		changed | (newClass
					subclassOf: sup
					oldClass: oldClass
					instanceVariableNames: instVarString
					variable: v
					words: w
					pointers: p
					ifBad: [^false]).
	invalidFields ifFalse: [newClass obsolete.  newClass _ oldClass].
	invalidMethods _ invalidFields | (newClass declare:  classVarString) | (newClass sharing: poolString).
	commentString == nil ifFalse: [newClass comment: commentString].
	(environ includesKey: newName)
		ifFalse:
			[environ declare: newName from: Undeclared.
			environ at: newName put: newClass].
	SystemOrganization classify: newClass name under: categoryName asSymbol.
	newClass
		validateFrom: oldClass
		in: environ
		instanceVariableNames: invalidFields
		methods: invalidMethods.
	"update subclass lists"
	newClass superclass removeSubclass: oldClass.
	newClass superclass addSubclass: newClass.
	"Update Changes"
	wasPresent
		ifTrue: [Smalltalk changes changeClass: newClass]
		ifFalse: [Smalltalk changes addClass: newClass].
	^newClass

"""
    // 69 .. 238
    let expected = [16, 211, 210, 168, 5, 112, 33, 224, 135, 122, 17, 16, 233, 129, 76, 172, 27, 17, 16, 192, 130, 77, 29, 72, 231, 154, 115, 164, 14, 112, 16, 38, 229, 224, 135, 114, 130, 76, 112, 16, 228, 129, 77, 148, 112, 16, 228, 129, 77, 135, 29, 218, 130, 78, 27, 30, 18, 29, 19, 20, 21, 22, 137, 117, 200, 164, 1, 122, 131, 236, 235, 130, 79, 31, 168, 6, 30, 221, 135, 29, 130, 78, 31, 30, 23, 238, 235, 30, 24, 239, 235, 130, 80, 26, 115, 198, 168, 5, 30, 26, 131, 48, 135, 17, 16, 233, 168, 11, 17, 16, 82, 131, 81, 135, 17, 16, 30, 193, 135, 84, 30, 131, 21, 25, 131, 22, 131, 83, 135, 30, 29, 17, 31, 128, 80, 131, 151, 135, 30, 131, 25, 29, 131, 56, 135, 30, 131, 25, 30, 131, 58, 135, 28, 158, 93, 131, 28, 30, 131, 62, 149, 93, 131, 28, 30, 131, 59, 135, 30, 124]
    try runningSource(source, expecting: expected)
  }

  func testScopeHasIfTrue() throws {
    let source = """
scopeHas: name ifTrue: assocBlock
	^thisClass scopeHas: name ifTrue: assocBlock

"""
    // 5 .. 9
    let expected = [6, 16, 17, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsMeta() throws {
    let source = """
isMeta
	^ true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testCopyForValidation() throws {
    let source = """
copyForValidation
	"Special copy for ClassDescription validateFrom:in:fields:methods:.  Answer a copy
	of the receiver without the subclasses."

	^super copy

"""
    // 7 .. 10
    let expected = [112, 133, 0, 124]
    try runningSource(source, expecting: expected)
  }

  func testNewNamed() throws {
    let source = """
newNamed: aSymbol
	"Answer a new instance of me whose name is the argument, aSymbol."
	^(Metaclass subclassOf: self) new
		superclass: Object
		methodDict: MethodDictionary new
		format: -8192
		name: aSymbol
		organization: ClassOrganizer new
		instVarNames: nil
		classPool: nil
		sharedPools: nil

"""
    // 17 .. 34
    let expected = [66, 112, 225, 204, 67, 68, 204, 37, 16, 70, 204, 115, 115, 115, 132, 8, 0, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstanceVariableNames() throws {
    let source = """
instanceVariableNames: instVarString
	"Declare additional variables for my instances."

	| newMeta invalid ok |
	newMeta _ self copyForValidation.
	invalid _ newMeta
				subclassOf: superclass
				oldClass: self
				instanceVariableNames: instVarString
				variable: false
				words: true
				pointers: true
				ifBad: [^false].
	invalid
		ifTrue:
			[ok _ newMeta
						validateFrom: self
						in: Smalltalk
						instanceVariableNames: true
						methods: true.
			Smalltalk changes changeClass: self.
			^ok]

"""
    // 15 .. 52
    let expected = [112, 208, 105, 17, 0, 112, 16, 114, 113, 113, 137, 117, 200, 164, 1, 122, 131, 225, 106, 18, 172, 15, 17, 112, 67, 113, 113, 131, 130, 107, 67, 213, 112, 228, 135, 19, 124, 120]
    try runningSource(source, expecting: expected)
  }

  func testNewNamedOtherSupers() throws {
    let source = """
newNamed: aSymbol otherSupers: others
	"Answer a new instance of me whose name is the argument, aSymbol."
	^ (MetaclassForMultipleInheritance subclassOf: self and: others) new
		superclass: Object
		methodDict: MethodDictionary new
		format: -8192
		name: aSymbol
		organization: ClassOrganizer new
		instVarNames: nil
		classPool: nil
		sharedPools: nil

"""
    // 17 .. 35
    let expected = [66, 112, 17, 241, 204, 67, 68, 204, 37, 16, 70, 204, 115, 115, 115, 132, 8, 0, 124]
    try runningSource(source, expecting: expected)
  }

  func testObsolete() throws {
    let source = """
obsolete
	"Invalidate and recycle local messages.  Remove the receiver from its superclass'
	subclass list."

	thisClass_ nil.
	super obsolete

"""
    // 7 .. 13
    let expected = [115, 102, 112, 133, 0, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsObsolete() throws {
    let source = """
isObsolete
	"Answer whether the receiver is an obsolete metaclass."

	^thisClass == nil or: [thisClass ~~ (Smalltalk at: thisClass name ifAbsent: [nil])]
	"should only be true for obsolete metaclass"

"""
    // 11 .. 31
    let expected = [6, 115, 198, 154, 113, 164, 13, 6, 66, 6, 211, 137, 117, 200, 164, 2, 115, 125, 241, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testNameInEnvironmentSubclassOfAndInstanceVariableNamesVariableWordsPointersClassVariableNamesPoolDictionariesCategoryCommentChanged() throws {
    let source = """
name: newName inEnvironment: environ subclassOf: sup and: others instanceVariableNames: instVarString variable: v words: w pointers: p classVariableNames: classVarString poolDictionaries: poolString category: categoryName comment: commentString changed: changed
	"Create a new metaclass from the information provided in the arguments.
	Create an error if the name does not begin with an uppercase letter or if a
	class of the same name already exists."

	| wasPresent oldClass newClass invalidFields invalidMethods |
	newName first isUppercase
		ifFalse:
			[self error: 'Class names must be capitalized'.
			^false].
	(wasPresent _ environ includesKey: newName)
		ifTrue:
			[oldClass _ environ at: newName.
			(oldClass isKindOf: Behavior)
				ifFalse:
					[self error: newName , ' already exists!  Proceed will store over it'.
					wasPresent _ false.
					oldClass _ self newNamed: newName otherSupers: others]]
		ifFalse: [oldClass _ self newNamed: newName otherSupers: others].
	newClass _ oldClass copy.
	invalidFields _
		changed | (newClass
					subclassOf: sup
					oldClass: oldClass
					instanceVariableNames: instVarString
					variable: v
					words: w
					pointers: p
					ifBad: [^false]).
	invalidFields ifFalse: [newClass obsolete.  newClass _ oldClass].
	invalidMethods _ invalidFields | (newClass declare:  classVarString) | (newClass sharing: poolString).
	commentString == nil ifFalse: [newClass comment: commentString].
	(environ includesKey: newName)
		ifFalse:
			[environ declare: newName from: Undeclared.
			environ at: newName put: newClass].
	SystemOrganization classify: newClass name under: categoryName asSymbol.
	newClass
		validateFrom: oldClass
		in: environ
		instanceVariableNames: invalidFields
		methods: invalidMethods.
	"update subclass lists"
	newClass superclasses do:
		[:sup | sup removeSubclass: oldClass; addSubclass: newClass].
	"Update Changes"
	wasPresent
		ifTrue: [Smalltalk changes changeClass: newClass]
		ifFalse: [Smalltalk changes addClass: newClass].
	"Now check for possible conflicting definitions in superclasses"
	invalidFields ifTrue:
		[newClass copyMethods.
		newClass class copyMethods].
	^newClass

"""
    // 71 .. 265
    let expected = [16, 211, 210, 168, 5, 112, 33, 224, 135, 122, 17, 16, 233, 129, 77, 172, 28, 17, 16, 192, 130, 78, 30, 72, 231, 154, 115, 164, 15, 112, 16, 38, 229, 224, 135, 114, 130, 77, 112, 16, 19, 244, 129, 78, 149, 112, 16, 19, 244, 129, 78, 135, 30, 218, 130, 79, 28, 31, 18, 30, 20, 21, 22, 23, 137, 117, 200, 164, 1, 122, 131, 236, 235, 130, 80, 128, 80, 168, 6, 31, 221, 135, 30, 130, 79, 128, 80, 31, 24, 238, 235, 31, 25, 239, 235, 130, 81, 27, 115, 198, 168, 5, 31, 27, 131, 48, 135, 17, 16, 233, 168, 11, 17, 16, 82, 131, 81, 135, 17, 16, 31, 193, 135, 84, 31, 131, 21, 26, 131, 22, 131, 83, 135, 31, 30, 17, 128, 80, 128, 81, 131, 151, 135, 31, 131, 24, 137, 118, 200, 164, 11, 106, 18, 136, 30, 131, 57, 135, 31, 131, 58, 125, 203, 135, 29, 158, 93, 131, 28, 31, 131, 62, 149, 93, 131, 28, 31, 131, 59, 135, 128, 80, 172, 9, 31, 131, 31, 135, 31, 199, 131, 31, 135, 31, 124]
    try runningSource(source, expecting: expected)
  }


  func testName() throws {
    let source = """
name
	"Answer a String that is the name of the receiver, either Metaclass or the
	name of the receiver's class followed by the ' class'."

	thisClass == nil
		ifTrue: [^'a Metaclass']
		ifFalse: [^thisClass name , ' class']

"""
    // 11 .. 21
    let expected = [6, 115, 198, 153, 35, 124, 6, 209, 34, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSuperclass() throws {
    let source = """
superclass: superMeta
	"Change the receiver's superclass to be the argument, superMeta, a metaclass."

	superclass _ superMeta

"""
    // 3 .. 5
    let expected = [16, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testDefinition() throws {
    let source = """
definition
	"Answer with a string that defines me"

	| aStream names |
	aStream _ WriteStream on: (String new: 300).
	self printOn: aStream.
	aStream nextPutAll: '
	instanceVariableNames: '''.
	names _ self instVarNames.
	1 to: names size do: [:i | aStream nextPutAll: (names at: i); space].
	aStream nextPut: $'.
	^ aStream contents

"""
    // 27 .. 70
    let expected = [65, 66, 35, 205, 224, 104, 112, 16, 228, 135, 16, 38, 229, 135, 112, 215, 105, 118, 17, 194, 137, 118, 200, 164, 10, 106, 16, 136, 17, 18, 192, 229, 135, 217, 125, 248, 135, 16, 42, 196, 135, 16, 219, 124]
    try runningSource(source, expecting: expected)
  }

  func testClassPool() throws {
    let source = """
classPool
	"Answer the dictionary of class variables."

	^thisClass classPool

"""
    // 5 .. 7
    let expected = [6, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddClassVarName() throws {
    let source = """
addClassVarName: aString
	^thisClass addClassVarName: aString

"""
    // 5 .. 8
    let expected = [6, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddInstVarName() throws {
    let source = """
addInstVarName: aString
	"Add the argument, aString, as one of the receiver's instance variables."

	| fullString |
	fullString _ aString.
	self instVarNames do: [:aString2 | fullString _ aString2 , ' ' , fullString].
	self instanceVariableNames: fullString

"""
    // 11 .. 35
    let expected = [16, 105, 112, 208, 137, 118, 200, 164, 9, 106, 18, 34, 225, 17, 225, 129, 65, 125, 203, 135, 112, 17, 227, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveInstVarName() throws {
    let source = """
removeInstVarName: aString
	"Remove the argument, aString, as one of the receiver's instance variables."


	| newArray newString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newArray _ self instVarNames copyWithout: aString.
	newString _ ''.
	newArray do: [:aString2 | newString _ aString2 , ' ' , newString].
	self instanceVariableNames: newString

"""
    // 21 .. 61
    let expected = [112, 212, 16, 227, 168, 6, 112, 16, 34, 225, 224, 135, 112, 212, 16, 229, 105, 38, 106, 17, 137, 118, 200, 164, 9, 107, 19, 39, 225, 18, 225, 129, 66, 125, 203, 135, 112, 18, 232, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
