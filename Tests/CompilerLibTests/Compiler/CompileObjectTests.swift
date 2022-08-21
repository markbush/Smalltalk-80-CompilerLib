import XCTest
@testable import CompilerLib

final class CompileObjectTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Object", instanceVariables: [])
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

  func testDoesNotUnderstand() throws {
    let source = """
doesNotUnderstand: aMessage
	"First check for a compound selector.  If found, try copying down code
	into the receiver's class.  If this is unsuccessful,
	announce that the receiver does not understand the argument, aMessage,
	as a message.  The default behavior is to create a Notifier containing the
	appropriate message and to allow the user to open a Debugger.
	Subclasses can override this message in order to modify this behavior."
	| status gripe |

	status _ self class tryCopyingCodeFor: aMessage selector.
	status==#OK ifTrue:
		[^self perform: aMessage selector withArguments: aMessage arguments].

	gripe _ status==#HierarchyViolation
		ifTrue: [aMessage selector classPart , ' is not one of my superclasses: ']
		ifFalse: ['Message not understood: '].
	NotifierView
		openContext: thisContext
		label: gripe , aMessage selector
		contents: thisContext shortStack.
	"Try the message again if the programmer decides to proceed."
	^self perform: aMessage selector withArguments: aMessage arguments

	"3 zork."

"""
    // 29 .. 75
    let expected = [112, 199, 16, 209, 224, 105, 17, 36, 198, 158, 112, 16, 209, 16, 211, 242, 124, 17, 41, 198, 157, 16, 209, 215, 40, 230, 144, 37, 106, 75, 137, 18, 16, 209, 230, 137, 220, 131, 106, 135, 112, 16, 209, 16, 211, 242, 124]
    try runningSource(source, expecting: expected)
  }

  func testMustBeBoolean() throws {
    let source = """
mustBeBoolean
	"Catches attempts to test truth of non-Booleans.  This message is sent from the
	interpreter."

	self error: 'NonBoolean receiver--proceed for truth.'.
	^true

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 121]
    try runningSource(source, expecting: expected)
  }

  func testUpdate() throws {
    let source = """
update: aParameter
	"Receive a change notice from an object of whom the receiver is a dependent.
	The default behavior is to do nothing;  a subclass might want to change
	itself in some way."

	^self

"""
    // 3 .. 3
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testPerformWith() throws {
    let source = """
perform: aSymbol with: anObject
	"Send the receiver the keyword message indicated by the arguments. The first
	argument is the selector of the message. The other argument is the
	argument of the message to be sent. Invoke messageNotUnderstood: if the
	number of arguments expected by the selector is not one. Optional. See
	documentation in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array with: anObject)

"""
    // 13 .. 19
    let expected = [112, 16, 66, 17, 225, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstVarAt() throws {
    let source = """
instVarAt: index
	"Answer a fixed variable in an object.  The numbering of the variables
	corresponds to the named instance variables.  Fail if the index is not an
	Integer or is not the index of a fixed variable.  Essential.  See documentation in
	Object metaclass."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize

"""
    // 11 .. 18
    let expected = [112, 16, 112, 199, 209, 177, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstVarAtPut() throws {
    let source = """
instVarAt: anInteger put: anObject
	"Store a value into a fixed variable in the receiver.  The numbering of the variables
	corresponds to the named instance variables.  Fail if the index is not an
	Integer or is not the index of a fixed variable.   Answer the value stored as the
	result.  Using this message violates the principle that each object has
	sovereign control over the storing of values into its instance variables.
	Essential.  See documentation in Object metaclass."

	<primitive: 74>
	"Access beyond fixed fields"
	^self basicAt: anInteger - self class instSize put: anObject

"""
    // 11 .. 19
    let expected = [112, 16, 112, 199, 209, 177, 17, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testNotEquivalent() throws {
    let source = """
~~ anObject
	"Answer true if the receiver and the argument are not the same object
	(have the same object pointer) and false otherwise."

	^(self == anObject) not

"""
    // 5 .. 9
    let expected = [112, 16, 198, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testSpecies() throws {
    let source = """
species
	"Answer the preferred class for reconstructing the receiver.  For example,
	collections create new collections whenever enumeration messages such as
	collect: or select: are invoked.  The new kind of collection is determined by
	the species of the original collection.  Species and class are not always the
	same.  For example, the species of Interval is Array."

	^self class

"""
    // 3 .. 5
    let expected = [112, 199, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubclassResponsibility() throws {
    let source = """
subclassResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message."

	self error: 'My subclass should have overridden one of my messages.'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testBasicAt() throws {
    let source = """
basicAt: index
	"Answer the value of an indexable field in the receiver. Fail if the
	argument index is not an Integer or is out of bounds. Essential. See
	documentation in Object metaclass.  Do not override this message in any
	subclass."

	<primitive: 60>
	(index isKindOf: Integer) ifTrue: [self errorSubscriptBounds: index].
	(index isKindOf: Number)
		ifTrue: [^self basicAt: index truncated]
		ifFalse: [self errorNonIntegerIndex]

"""
    // 21 .. 41
    let expected = [16, 66, 225, 155, 112, 16, 224, 135, 16, 70, 225, 156, 112, 16, 213, 228, 124, 112, 211, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPerformWithWith() throws {
    let source = """
perform: aSymbol with: firstObject with: secondObject
	"Send the receiver the keyword message indicated by the arguments. The first
	argument is the selector of the message. The other arguments are the
	arguments of the message to be sent. Invoke messageNotUnderstood: if
	the number of arguments expected by the selector is not two. Optional.
	See documentation in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)

"""
    // 13 .. 20
    let expected = [112, 16, 66, 17, 18, 241, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testRespondsTo() throws {
    let source = """
respondsTo: aSymbol
	"Answer a Boolean as to whether the method dictionary of the receiver's class
	contains aSymbol as a message selector."

	^self class canUnderstand: aSymbol

"""
    // 5 .. 9
    let expected = [112, 199, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testBecome() throws {
    let source = """
become: otherObject
	"Swap the instance pointers of the receiver and the argument, otherObject.  All
	variables in the entire system that used to point to the receiver now point to the
	argument, and vice-versa.  Fails if either object in a SmallInteger.  Answer the
	argument which is now the same instance pointer that formerly denoted the receiver.  	Essential.  See documentation in Object metaclass."

	<primitive: 72>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	"Answer the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See documentation in Object
	metaclass. "

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0

"""
    // 7 .. 8
    let expected = [117, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	"Append to the argument aStream a sequence of characters that identifies the receiver."

	| title |
	title _ self class name.
	aStream nextPutAll: ((title at: 1) isVowel
							ifTrue: ['an ']
							ifFalse: ['a '])
						, title

"""
    // 15 .. 32
    let expected = [112, 199, 208, 105, 16, 17, 118, 192, 213, 153, 36, 144, 35, 17, 226, 225, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testEquivalent() throws {
    let source = """
== anObject
	"Answer true if the receiver and the argument are the same object (have the same
	object pointer) and false otherwise.  Do not redefine the message == in any
	other class!  Essential.  No Lookup.  See documentation in Object metaclass."

	<primitive: 110>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNotEquals() throws {
    let source = """
~= anObject
	"Answer true if the receiver and the argument do not represent the same
	object and false otherwise."

	^self = anObject == false

"""
    // 3 .. 8
    let expected = [112, 16, 182, 114, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorImproperStore() throws {
    let source = """
errorImproperStore
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testInspect() throws {
    let source = """
inspect
	"Create and schedule an Inspector in which the user can examine the
	receiver's variables."
	self basicInspect

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testChangeRequest() throws {
    let source = """
changeRequest
	"Receiver wants to change; check with all dependents that it is OK."

	self dependents do: [:dep | dep updateRequest ifFalse: [^false]].
	^true

"""
    // 7 .. 24
    let expected = [112, 208, 137, 118, 200, 164, 8, 104, 16, 209, 153, 115, 144, 122, 125, 203, 135, 121]
    try runningSource(source, expecting: expected)
  }

  func testChangeRequestFrom() throws {
    let source = """
changeRequestFrom: requestor
	"Receiver wants to change; check with all dependents (other than requestor) that it is OK."

	self dependents do: [:dep | (dep == requestor or: [dep updateRequest]) ifFalse: [^false]].
	^true

"""
    // 7 .. 30
    let expected = [112, 208, 137, 118, 200, 164, 14, 105, 17, 16, 198, 153, 113, 145, 17, 209, 153, 115, 144, 122, 125, 203, 135, 121]
    try runningSource(source, expecting: expected)
  }

  func testShouldNotImplement() throws {
    let source = """
shouldNotImplement
	"Announce that although the receiver inherits this message, it
	should not implement it."

	self error: 'This message is not appropriate for this object'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testBroadcastWith() throws {
    let source = """
broadcast: aSymbol with: anObject
	"Send the argument, aSymbol, as a keyword message with argument anObject to
	all of the receiver's dependents."

	self dependents ~~ nil
		ifTrue: [self dependents do:
					[:aDependent | aDependent perform: aSymbol with: anObject]]

"""
    // 9 .. 30
    let expected = [112, 208, 115, 226, 172, 15, 112, 208, 137, 118, 200, 164, 6, 106, 18, 16, 17, 241, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHalt() throws {
    let source = """
halt
	"This is a simple message to use for inserting breakpoints during debugging."

	NotifierView
		openContext: thisContext
		label: 'Halt encountered.'
		contents: thisContext shortStack

	"nil halt."

"""
    // 11 .. 19
    let expected = [65, 137, 34, 137, 211, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Answer a SmallInteger whose value is half of the receiver's object pointer
	(interpreting object pointers as 16-bit signed quantities).  Fails if the
	receiver is a SmallInteger.  Essential.  See documentation in Object metaclass."

	<primitive: 75>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPerform() throws {
    let source = """
perform: aSymbol
	"Send the receiver the unary message indicated by the argument. The argument is
	the selector of the message. Invoke messageNotUnderstood: if the number of
	arguments expected by the selector is not zero. Optional. See documentation
	in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array new: 0)

"""
    // 11 .. 17
    let expected = [112, 16, 65, 117, 205, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetDependents() throws {
    let source = """
setDependents
	"Allocate the soft field for the receiver's dependents."
	| dependents |
	dependents _ OrderedCollection new.
	DependentsFields add: (Association key: self value: dependents).
	^ dependents

"""
    // 13 .. 24
    let expected = [64, 204, 104, 66, 68, 112, 16, 243, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testDependents() throws {
    let source = """
dependents
	"Answer an OrderedCollection of the objects that are dependent on the
	receiver, that is, the objects that should be notified if the receiver changes."

	^ DependentsFields at: self ifAbsent: [OrderedCollection new]

"""
    // 9 .. 20
    let expected = [65, 112, 137, 117, 200, 164, 3, 66, 204, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testBreakDependents() throws {
    let source = """
breakDependents
	"Deallocate the soft field for the receiver's dependents."

	DependentsFields removeKey: self ifAbsent: []

"""
    // 7 .. 18
    let expected = [65, 112, 137, 117, 200, 164, 2, 115, 125, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	"Answer another instance just like the receiver. Subclasses typically override
	this method;  they typically do not override shallowCopy"

	^self shallowCopy

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveDependent() throws {
    let source = """
removeDependent: anObject
	"Remove the argument, anObject, as one of the receiver's dependents."
	| dependents |
	dependents _ DependentsFields at: self ifAbsent: [^ anObject].
	dependents remove: anObject ifAbsent: [].
	dependents isEmpty ifTrue: [self breakDependents].
	^anObject

"""
    // 13 .. 42
    let expected = [65, 112, 137, 117, 200, 164, 2, 16, 124, 240, 105, 17, 16, 137, 117, 200, 164, 2, 115, 125, 242, 135, 17, 212, 154, 112, 211, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRelease() throws {
    let source = """
release
	"Remove references to objects that may refer to the receiver. Answers self.
	This message should be overidden by subclasses with any cycles, in which
	case the subclass should also include the expression super release."

	self breakDependents

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= anObject
	"Answer true if the receiver and the argument represent the same object
	and false otherwise. If = is redefined in any subclass, consider also
	redefining the message hash."

	^self == anObject

"""
    // 3 .. 6
    let expected = [112, 16, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testTryPrimitive0() throws {
    let source = """
tryPrimitive0
	"Warning!! This is not a real primitive.  This method is a template that the
	Smalltalk simulator uses to execute primitives with no arguments.  See
	ContextPart class initPrimitives and ContextPart doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail

"""
    // 9 .. 10
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testTryPrimitive1() throws {
    let source = """
tryPrimitive1: arg1
	"Warning!! This is not a real primitive.  This method is a template that the
	Smalltalk simulator uses execute primitives with one argument.  See
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail

"""
    // 9 .. 10
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testTryPrimitive2With() throws {
    let source = """
tryPrimitive2: arg1 with: arg2
	"Warning!! This is not a real primitive.  This method is a template that the
	Smalltalk simulator uses execute primitives with two arguments.  See
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail

"""
    // 9 .. 10
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testTryPrimitive3WithWith() throws {
    let source = """
tryPrimitive3: arg1 with: arg2 with: arg3
	"Warning!! This is not a real primitive.  This method is a template that the
	Smalltalk simulator uses execute primitives with three arguments.  See
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail

"""
    // 9 .. 10
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDeepCopy() throws {
    let source = """
deepCopy
	"Answer a copy of the receiver with its own copy of each instance variable."

	| newObject class index |
	class _ self class.
	(class == Object) ifTrue: [^self].
	class isVariable
		ifTrue:
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue:
					[newObject basicAt: index put: (self basicAt: index) deepCopy.
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue:
			[newObject instVarAt: index put: (self instVarAt: index) deepCopy.
			index _ index - 1].
	^newObject

"""
    // 25 .. 93
    let expected = [112, 199, 105, 17, 64, 198, 152, 120, 17, 215, 172, 28, 112, 210, 106, 17, 18, 227, 104, 18, 117, 179, 172, 14, 16, 18, 112, 18, 230, 213, 244, 135, 18, 118, 177, 106, 163, 237, 115, 147, 17, 209, 129, 64, 135, 17, 216, 106, 18, 117, 179, 172, 14, 16, 18, 112, 18, 234, 213, 249, 135, 18, 118, 177, 106, 163, 237, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testTryPrimitive4WithWithWith() throws {
    let source = """
tryPrimitive4: arg1 with: arg2 with: arg3 with: arg4
	"Warning!! This is not a real primitive.  This method is a template that the
	Smalltalk simulator uses execute primitives with four arguments.  See
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail

"""
    // 9 .. 10
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsMemberOf() throws {
    let source = """
isMemberOf: aClass
	"Answer a Boolean as to whether the receiver is an instance of the class, aClass."

	^self class == aClass

"""
    // 3 .. 7
    let expected = [112, 199, 16, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsInteger() throws {
    let source = """
isInteger
	"Coerces Integers to true and everything else to false.  Integer
	overrides with ^true"

	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testAsOop() throws {
    let source = """
asOop
	"Answer an Integer which is unique to me.  See below.
	Essential.  See also documentation in Object metaclass."
	<primitive: 75>
	self primitiveFailed
	"
	Non-Stretch mapping between objects and asOop value:
	oops 0...16K-1			-->	0...16K-1
	oops 16K...32K-1			-->	-16K...-1
	SmallIntegers 0...16K		-->	32K...48K-1
	SmallIntegers -16K...0	-->	48K...64K-1

	Stretch mapping between objects and asOop value:
	oops 0...48K-1			-->	0...48K-1
	SmallIntegers -8K...-1	-->	-16K...-2 even
	SmallIntegers 0...8K-1	-->	-(16K-1)...-1 odd

	Non-Stretch Consistency check:
	| obj |
	-16384 to: 16383 do:
		[:i | obj _ i asObject.
		(obj == #NonExistentObject or: [obj asOop = i])
			ifFalse: [self error: 'inconsistency']].
	32768 to: 65536 do:
		[:i | obj _ i asObject.
		(obj == #NonExistentObject or: [obj asOop = i])
			ifFalse: [self error: 'inconsistency']].

	Stretch Consistency check:
	| obj |
	-16384 to: 49151 do:
		[:i | obj _ i asObject.
		(obj == #NonExistentObject or: [obj asOop = i])
			ifFalse: [self error: 'inconsistency']].
	"

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNotNil() throws {
    let source = """
notNil
	"Coerces nil to false and everything else to true.  UndefinedObject
	overrides with ^false"

	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testForkEmergencyEvaluatorAt() throws {
    let source = """
forkEmergencyEvaluatorAt: priority
	"Fork a process running a simple Smalltalk evaluator using as little of the system as possible.  Used for desperate debugging.  may be invoked by control-shift-C."

	| stream char |
		[Display white: (0@0 extent: 1024@36).
		'EMERGENCY EVALUATOR (priority ', priority printString, ') -- type an expression terminated by ESC' displayAt: 50@0.
		Display reverse: (0@0 extent: 1024@36).
		stream _ WriteStream on: String new.
		[[Sensor keyboardPressed] whileFalse.
		(char _ Sensor keyboard) = 160 asCharacter] whileFalse:
			[char = Character backspace
				ifTrue: [stream skip: -1.  Display black: (0@18 extent: 1024@18)]
				ifFalse: [stream nextPut: char].
			stream contents displayAt: 50@18].
		Display black: (0@0 extent: 1024@18).
		(Compiler evaluate: stream contents) printString displayAt: 50@0] forkAt: priority

"""
    // 63 .. 190
    let expected = [137, 117, 200, 164, 119, 66, 117, 117, 187, 36, 37, 187, 227, 225, 135, 40, 16, 217, 231, 42, 231, 43, 117, 187, 230, 135, 66, 117, 117, 187, 36, 37, 187, 227, 236, 135, 78, 79, 204, 237, 105, 137, 117, 200, 164, 4, 88, 131, 23, 125, 131, 22, 135, 88, 131, 25, 129, 66, 59, 131, 26, 182, 168, 37, 18, 84, 131, 19, 182, 172, 16, 17, 116, 131, 48, 135, 66, 117, 50, 187, 36, 50, 187, 227, 131, 49, 146, 17, 18, 196, 135, 17, 131, 21, 43, 50, 187, 230, 135, 163, 196, 66, 117, 117, 187, 36, 50, 187, 227, 131, 49, 135, 93, 17, 131, 21, 131, 60, 217, 43, 117, 187, 230, 125, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testConflictingInheritanceError() throws {
    let source = """
conflictingInheritanceError
	"Browse to the method which called this,
		redefine it appropriately,
		and then restart that calling method. "

	self error: 'Conflicting methods due to multiple inheritance'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNilFields() throws {
    let source = """
nilFields
	"Store nil into all pointer fields of the receiver."

	self class isPointers ifFalse: [^self].
	1 to: self basicSize do:
		[:index | self basicAt: index put: nil].
	1 to: self class instSize do:
		[:index | self instVarAt: index put: nil].

"""
    // 15 .. 54
    let expected = [112, 199, 208, 168, 1, 120, 118, 112, 210, 137, 118, 200, 164, 6, 104, 112, 16, 115, 243, 125, 241, 135, 118, 112, 199, 212, 137, 118, 200, 164, 6, 104, 112, 16, 115, 245, 125, 241, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrimitiveError() throws {
    let source = """
primitiveError: aString
	"This method is called when the error handling results in a recursion in calling
	on error: or halt or halt:."

	| context key |
	Transcript cr.
	Transcript show: '**System Error Handling Failed** '.
	Transcript show: aString.
	Transcript cr.
	context _ thisContext sender sender.
	3 timesRepeat:
		[context == nil ifFalse: [Transcript print: (context _ context sender); cr]].

	[Transcript show: '**type <s> for more stack; anything else restarts scheduler**'.
	Transcript cr.
	key _ Sensor keyboard.
	key = $s | (key = $S)]
		whileTrue:
			[5 timesRepeat:
				[context == nil
					ifFalse: [Transcript print: (context _ context sender); cr]]].
	ScheduledControllers searchForActiveController

"""
    // 37 .. 129
    let expected = [65, 208, 135, 65, 35, 226, 135, 65, 16, 226, 135, 65, 208, 135, 137, 212, 212, 105, 38, 137, 117, 200, 164, 17, 17, 115, 198, 154, 115, 164, 9, 65, 136, 17, 212, 129, 65, 231, 135, 208, 125, 229, 135, 65, 41, 226, 135, 65, 208, 135, 75, 218, 106, 18, 45, 182, 18, 46, 182, 236, 172, 27, 40, 137, 117, 200, 164, 17, 17, 115, 198, 154, 115, 164, 9, 65, 136, 17, 212, 129, 65, 231, 135, 208, 125, 229, 135, 163, 210, 80, 223, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testBasicAtPut() throws {
    let source = """
basicAt: index put: value
	"Store the second argument value in the indexable field of the receiver
	indicated by index. Fail if the index is not an Integer or is out of bounds. Or
	fail if the value is not of the right type for this kind of collection. Answer
	the value that was stored. Essential. See documentation in Object
	metaclass. Do not override in a subclass."

	<primitive: 61>
	(index isKindOf: Integer)
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	(index isKindOf: Number)
		ifTrue: [^self basicAt: index truncated put: value]
		ifFalse: [self errorNonIntegerIndex]

"""
    // 23 .. 59
    let expected = [16, 67, 226, 172, 18, 16, 118, 181, 156, 16, 112, 194, 180, 144, 114, 154, 112, 209, 146, 112, 16, 224, 135, 16, 71, 226, 157, 112, 16, 214, 17, 245, 124, 112, 212, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"Append to the argument aStream a sequence of characters that is an expression
	whose evaluation creates an object similar to the receiver."

	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(', self class name, ' basicNew: ';
					store: self basicSize;
					nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name, ' basicNew'].
	1 to: self class instSize do:
		[:i |
		aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	1 to: self basicSize do:
		[:i |
		aStream nextPutAll: ' basicAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self basicAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'

"""
    // 43 .. 151
    let expected = [16, 32, 196, 135, 112, 199, 218, 172, 19, 16, 136, 37, 112, 199, 211, 226, 38, 226, 225, 135, 136, 112, 216, 231, 135, 41, 225, 150, 16, 112, 199, 211, 36, 226, 225, 135, 118, 112, 199, 220, 137, 118, 200, 164, 23, 105, 16, 136, 45, 225, 135, 136, 17, 231, 135, 136, 46, 225, 135, 136, 112, 17, 239, 231, 135, 48, 196, 125, 251, 135, 118, 112, 216, 137, 118, 200, 164, 24, 105, 16, 136, 49, 225, 135, 136, 17, 231, 135, 136, 46, 225, 135, 136, 112, 17, 131, 50, 231, 135, 48, 196, 125, 251, 135, 16, 51, 225, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testClass() throws {
    let source = """
class
	"Answer the object which is the receiver's class. Essential.  See
	documentation in Object metaclass."

	<primitive: 111>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextInstance() throws {
    let source = """
nextInstance
	"Answer the next instance after the receiver in the enumeration of all
	instances of this class.  Fails if all instances have been enumerated.
	Essential.  See documentation in Object metaclass."

	<primitive: 78>
	^nil

"""
    // 7 .. 7
    let expected = [123]
    try runningSource(source, expecting: expected)
  }

  func testPrintString() throws {
    let source = """
printString
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream.
	^aStream contents

"""
    // 15 .. 27
    let expected = [65, 66, 35, 205, 224, 104, 112, 16, 228, 135, 16, 213, 124]
    try runningSource(source, expecting: expected)
  }


  func testShallowCopy() throws {
    let source = """
shallowCopy
	"Answer a copy of the receiver which shares the receiver's instance
	variables. "

	| class newObject index |
	class _ self class.
	"I don't understand why the following check is here.  Object is not
	supposed to have any instances at all."
	class == Object ifTrue: [^self].
	class isVariable
		ifTrue:
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue:
					[newObject basicAt: index put: (self basicAt: index).
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue:
			[newObject instVarAt: index put: (self instVarAt: index).
			index _ index - 1].
	^newObject

"""
    // 23 .. 89
    let expected = [112, 199, 104, 16, 64, 198, 152, 120, 16, 214, 172, 27, 112, 210, 106, 16, 18, 227, 105, 18, 117, 179, 172, 13, 17, 18, 112, 18, 229, 244, 135, 18, 118, 177, 106, 163, 238, 115, 147, 16, 209, 129, 65, 135, 16, 215, 106, 18, 117, 179, 172, 13, 17, 18, 112, 18, 233, 248, 135, 18, 118, 177, 106, 163, 238, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreString() throws {
    let source = """
storeString
	"Answer a String representation of the receiver from which the receiver
	can be reconstructed."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self storeOn: aStream.
	^aStream contents

"""
    // 15 .. 27
    let expected = [65, 66, 35, 205, 224, 104, 112, 16, 228, 135, 16, 213, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorSubscriptBounds() throws {
    let source = """
errorSubscriptBounds: index
	"Create an error notification that an improper integer was used as an index."

	self error: 'subscript is out of bounds: ' , index printString

"""
    // 11 .. 18
    let expected = [112, 34, 16, 211, 225, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testBasicSize() throws {
    let source = """
basicSize
	"Answer the number of indexable fields in the receiver. This value is the
	same as the largest legal subscript. Essential. See documentation in Object
	metaclass. Do not override in any subclass."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0

"""
    // 7 .. 8
    let expected = [117, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrimitiveFailed() throws {
    let source = """
primitiveFailed
	"Announce that a primitive has failed and there is no appropriate
	Smalltalk code to run."

	self error: 'a primitive has failed'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPerformWithWithWith() throws {
    let source = """
perform: aSymbol with: firstObject with: secondObject with: thirdObject
	"Send the receiver the keyword message indicated by the arguments. The first
	argument is the selector of the message. The other arguments are the
	arguments of the message to be sent. Invoke messageNotUnderstood: if
	the number of arguments expected by the selector is not three. Optional.
	See documentation in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array
			with: firstObject
			with: secondObject
			with: thirdObject)

"""
    // 13 .. 22
    let expected = [112, 16, 66, 17, 18, 19, 131, 97, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testBroadcast() throws {
    let source = """
broadcast: aSymbol
	"Send the argument, aSymbol, as a unary message to all of the receiver's dependents."

	self dependents ~~ nil
		ifTrue: [self dependents do:
					[:aDependent | aDependent perform: aSymbol]]

"""
    // 9 .. 29
    let expected = [112, 208, 115, 226, 172, 14, 112, 208, 137, 118, 200, 164, 5, 105, 17, 16, 225, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: index
	"Answer the value of an indexable field in the receiver. Fail if the
	argument index is not an Integer or is out of bounds. Essential. See
	documentation in Object metaclass."

	<primitive: 60>
	index isInteger
		ifTrue: [self errorSubscriptBounds: index].
	(index isKindOf: Number)
		ifTrue: [^self at: index truncated]
		ifFalse: [self errorNonIntegerIndex]

"""
    // 19 .. 38
    let expected = [16, 209, 155, 112, 16, 224, 135, 16, 69, 228, 156, 112, 16, 211, 192, 124, 112, 210, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: index put: value
	"Store the argument value in the indexable field of the receiver indicated by
	index. Fail if the index is not an Integer or is out of bounds. Or fail if the
	value is not of the right type for this kind of collection. Answer the
	value that was stored. Essential. See documentation in Object metaclass."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	(index isKindOf: Number)
		ifTrue: [^self at: index truncated put: value]
		ifFalse: [self errorNonIntegerIndex]

"""
    // 21 .. 56
    let expected = [16, 210, 172, 18, 16, 118, 181, 156, 16, 112, 194, 180, 144, 114, 154, 112, 209, 146, 112, 16, 224, 135, 16, 70, 229, 157, 112, 16, 212, 17, 193, 124, 112, 211, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHaltWithArg() throws {
    let source = """
halt: aString
	"This message can be used for inserting breakpoints during debugging.
	It creates and schedules a Debugger with the argument, aString, as the label."

	NotifierView
		openContext: thisContext
		label: aString
		contents: thisContext shortStack

	"nil halt: 'Test of halt:.'."

"""
    // 9 .. 17
    let expected = [65, 137, 16, 137, 210, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testError() throws {
    let source = """
error: aString
	"The default behavior for error: is the same as halt:.
	This additional message is the one a subclass should override in order to
	change the handling of errors."

	NotifierView
		openContext: thisContext
		label: aString
		contents: thisContext shortStack

	"nil error: 'error message'."

"""
    // 9 .. 17
    let expected = [65, 137, 16, 137, 210, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testConfirm() throws {
    let source = """
confirm: aString
	"Create and start up a BinaryChoice menu with the argument as the message in order
	to determine true or false.  Answers true or false."
	| answer |
	answer _ false.
	BinaryChoice
		message: aString
		displayAt: Sensor cursorPoint
		centered: true
		ifTrue: [answer _ true]
		ifFalse: [answer _ false].
	^answer

"""
    // 11 .. 40
    let expected = [114, 105, 65, 16, 67, 210, 113, 137, 117, 200, 164, 4, 113, 129, 65, 125, 137, 117, 200, 164, 4, 114, 129, 65, 125, 131, 160, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testNotify() throws {
    let source = """
notify: aString
	"Create and schedule a Notifier with the argument as the message in order
	to request confirmation before a process can proceed."


	NotifierView
		openContext: thisContext
		label: 'Notifier'
		contents: aString

	"nil notify: 'confirmation message'."

"""
    // 9 .. 16
    let expected = [65, 137, 34, 16, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testUpdateRequest() throws {
    let source = """
updateRequest
	"Default behavior is to grant update requests;  a subclass might want to override
	this behavior if it is in the middle of making another change."

	^ true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testHashMappedBy() throws {
    let source = """
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map"
	^ map newHashFor: self hash

"""
    // 7 .. 11
    let expected = [16, 112, 209, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsLiteral() throws {
    let source = """
isLiteral
	"Answer whether the receiver has a literal text form recognized by the compiler."

	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testReadFromString() throws {
    let source = """
readFromString: aString
	"Create an object based on the contents of aString."

	^self readFrom: (ReadStream on: aString)

"""
    // 9 .. 14
    let expected = [112, 66, 16, 225, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testBasicInspect() throws {
    let source = """
basicInspect
	"Create and schedule an Inspector in which the user can examine the
	receiver's variables.  This method should not be overwritten."

	InspectorView open: (Inspector inspect: self)

"""
    // 11 .. 17
    let expected = [65, 67, 112, 226, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testErrorNonIntegerIndex() throws {
    let source = """
errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testChanged() throws {
    let source = """
changed
	"Receiver changed in a general way; inform all the dependents by sending
	each dependent an update: message."

	self changed: self

"""
    // 5 .. 9
    let expected = [112, 112, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddDependent() throws {
    let source = """
addDependent: anObject
	"Add anObject as one of the receiver's dependents."
	(DependentsFields at: self ifAbsent: [self setDependents])
		add: anObject.
	^anObject

"""
    // 11 .. 26
    let expected = [66, 112, 137, 117, 200, 164, 3, 112, 211, 125, 241, 16, 224, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsKindOf() throws {
    let source = """
isKindOf: aClass
	"Answer a Boolean as to whether the class, aClass, is a superclass or class of
	the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]

"""
    // 5 .. 15
    let expected = [112, 199, 16, 198, 152, 121, 112, 199, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testPerformWithArguments() throws {
    let source = """
perform: selector withArguments: anArray
	"Send the receiver the keyword message indicated by the arguments. The argument
	selector is the selector of the message. The arguments of the message are the
	elements of anArray. Invoke messageNotUnderstood: if the number of
	arguments expected by the selector is not the same as the length of
	anArray. Essential. See documentation in Object metaclass."

	<primitive: 84>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testChangedWith() throws {
    let source = """
changed: aParameter
	"Receiver changed.  The change is denoted by the argument aParameter.
	Usually the argument is a Symbol that is part of the dependent's change
	protocol.  Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter]

"""
    // 7 .. 21
    let expected = [112, 208, 137, 118, 200, 164, 5, 105, 17, 16, 225, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsNil() throws {
    let source = """
isNil
	"Coerces nil to true and everything else to false.  UndefinedObject
	overrides with ^true"

	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

}
