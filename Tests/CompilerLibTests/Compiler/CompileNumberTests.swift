import XCTest
@testable import CompilerLib

final class CompileNumberTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Number", instanceVariables: [])
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

  func testToDo() throws {
    let source = """
to: stop do: aBlock
	"Create an Interval from the receiver up to the argument, stop,
	incrementing by 1.  For each element of the interval, evaluate the
	block, aBlock."

	| nextValue |
	nextValue _ self.
	[nextValue <= stop]
		whileTrue:
			[aBlock value: nextValue.
			nextValue _ nextValue + 1]

"""
    // 3 .. 20
    let expected = [112, 106, 18, 16, 180, 172, 10, 17, 18, 202, 135, 18, 118, 176, 106, 163, 241, 120]
    try runningSource(source, expecting: expected)
  }

  func testTruncated() throws {
    let source = """
truncated
	"Answer an integer nearest the receiver toward zero."
	^self quo: 1

"""
    // 5 .. 8
    let expected = [112, 118, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testRem() throws {
    let source = """
rem: aNumber
	"Remainder defined in terms of quo:. Answer a Number with the same
	sign as self.  e.g. 9 rem: 4 = 1,  -9 rem: 4 = -1.  0.9 rem: 0.4 = 0.1"

	^self - ((self quo: aNumber) * aNumber)

"""
    // 5 .. 12
    let expected = [112, 112, 16, 224, 16, 184, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testQuo() throws {
    let source = """
quo: aNumber
	"Integer quotient defined by division with truncation toward zero.
	 -9 quo: 4 = -2,  -0.9 quo: 0.4 = -2.
	rem: answers the remainder from this division."

	^(self / aNumber) truncated

"""
    // 5 .. 9
    let expected = [112, 16, 185, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"Numbers print in a form which is recognized by the compiler."

	self printOn: aStream

"""
    // 5 .. 9
    let expected = [112, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRadiansToDegrees() throws {
    let source = """
radiansToDegrees
	"The receiver is assumed to represent radians.  Answer the
	conversion to degrees."
	^self asFloat radiansToDegrees

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testTruncateTo() throws {
    let source = """
truncateTo: aNumber
	"Answer the next multiple of aNumber toward zero that is nearest the receiver."

	^(self quo: aNumber)
		* aNumber

"""
    // 5 .. 10
    let expected = [112, 16, 224, 16, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testArcSin() throws {
    let source = """
arcSin
	"Answer with the angle in radians."

	^self asFloat arcSin

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsPoint() throws {
    let source = """
asPoint
	"Answer a new Point with the receiver as both coordinates;
	often used to supply the same value in two dimensions, as with
	symmetrical gridding or scaling."

	^self @ self

"""
    // 3 .. 6
    let expected = [112, 112, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testSqrt() throws {
    let source = """
sqrt
	"Answer the square root of the receiver."
	^self asFloat sqrt

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testNegated() throws {
    let source = """
negated
	"Answer a Number that is the negation of the receiver."
	^0 - self

"""
    // 3 .. 6
    let expected = [117, 112, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testArcTan() throws {
    let source = """
arcTan
	"Answer with the angle in radians."

	^self asFloat arcTan

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testStrictlyPositive() throws {
    let source = """
strictlyPositive
	"Answer whether the receiver is greater than 0."

	^self > 0

"""
    // 3 .. 6
    let expected = [112, 117, 179, 124]
    try runningSource(source, expecting: expected)
  }

  func testExp() throws {
    let source = """
exp
	"Answer the exponential of the receiver as a floating point number."
	^self asFloat exp

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testSquared() throws {
    let source = """
squared
	"Answer the receiver multipled by itself."
	^self * self

"""
    // 3 .. 6
    let expected = [112, 112, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testLn() throws {
    let source = """
ln
	"Answer the natural log of the receiver."
	^self asFloat ln

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testRaisedToInteger() throws {
    let source = """
raisedToInteger: anInteger
	"Answer the receiver raised to the power anInteger where the
	argument must be a kind of Integer.  This is a special case of raisedTo:."

	anInteger isInteger
		ifFalse: [^self error: 'raisedToInteger: only works for integral arguments'].
	anInteger = 0 ifTrue: [^1].
	anInteger = 1 ifTrue: [^self].
	anInteger > 1
		ifTrue: [^(self * self raisedToInteger: anInteger // 2)
					* (self raisedToInteger: anInteger \\\\ 2)].
	^(self raisedToInteger: anInteger negated) reciprocal

"""
    // 15 .. 58
    let expected = [16, 210, 168, 4, 112, 33, 224, 124, 16, 117, 182, 153, 118, 124, 16, 118, 182, 152, 120, 16, 118, 179, 172, 14, 112, 112, 184, 16, 119, 189, 227, 112, 16, 119, 186, 227, 184, 124, 112, 16, 213, 227, 212, 124]
    try runningSource(source, expecting: expected)
  }

  func testMultiply() throws {
    let source = """
* aNumber
	"Answer the result of multiplying the receiver by aNumber."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMod() throws {
    let source = """
\\\\ aNumber
	"modulo.  Remainder defined in terms of //.  Answer a Number with the
	same sign as aNumber.  e.g.  9\\4 = 1,  -9\\4 = 3, 9\\-4 =  -3,  0.9\\0.4 = 0.1"

	^self - (self // aNumber * aNumber)

"""
    // 3 .. 10
    let expected = [112, 112, 16, 189, 16, 184, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testAbs() throws {
    let source = """
abs
	"Answer a Number that is the absolute value (positive magnitude) of the receiver."

	self < 0
		ifTrue: [^self negated]
		ifFalse: [^self]

"""
    // 5 .. 12
    let expected = [112, 117, 178, 154, 112, 208, 124, 120]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ aNumber
	"Answer the result of dividing receiver by aNumber."
	self subclassResponsibility
"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRoundTo() throws {
    let source = """
roundTo: aNumber
	"Answer the integer that is a multiple of aNumber that is nearest the receiver."
	^(self / aNumber) rounded * aNumber

"""
    // 5 .. 11
    let expected = [112, 16, 185, 208, 16, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testDiv() throws {
    let source = """
// aNumber
	"Integer quotient defined by division with truncation toward negative
	infinity.  9//4 = 2,  -9//4 = -3.  -0.9//0.4 = -3.
	\\ answers the remainder from this division."

	^(self / aNumber) floor
"""
    // 5 .. 9
    let expected = [112, 16, 185, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testPlus() throws {
    let source = """
+ aNumber
	"Answer the sum of the receiver and aNumber."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFloor() throws {
    let source = """
floor
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation _ self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]

"""
    // 5 .. 23
    let expected = [112, 208, 104, 112, 117, 181, 153, 16, 124, 112, 16, 182, 153, 16, 124, 16, 118, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testCeiling() throws {
    let source = """
ceiling
	"Answer the integer nearest the receiver toward positive infinity."

	self <= 0.0
		ifTrue: [^self truncated]
		ifFalse: [^self negated floor negated]

"""
    // 11 .. 22
    let expected = [112, 35, 180, 154, 112, 210, 124, 112, 208, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testPointAt() throws {
    let source = """
@ y
	"Answer a new Point whose x value is the receiver and whose y value is the
	argument.  Optional.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 18>
	^Point x: self y: y

"""
    // 11 .. 15
    let expected = [65, 112, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testToBy() throws {
    let source = """
to: stop by: step
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by step."

	^Interval from: self to: stop by: step

"""
    // 7 .. 13
    let expected = [65, 112, 16, 17, 131, 96, 124]
    try runningSource(source, expecting: expected)
  }

  func testToByDo() throws {
    let source = """
to: stop by: step do: aBlock
	"Create an Interval from the receiver up to the argument, stop,
	incrementing by step.  For each element of the interval, evaluate the
	block, aBlock."

	| nextValue |
	nextValue _ self.
	step < 0
		ifTrue: [[stop <= nextValue]
				whileTrue:
					[aBlock value: nextValue.
					nextValue _ nextValue + step]]
		ifFalse: [[stop >= nextValue]
				whileTrue:
					[aBlock value: nextValue.
					nextValue _ nextValue + step]]

"""
    // 3 .. 45
    let expected = [112, 107, 17, 117, 178, 172, 18, 16, 19, 180, 172, 10, 18, 19, 202, 135, 19, 17, 176, 107, 163, 241, 115, 164, 16, 16, 19, 181, 172, 10, 18, 19, 202, 135, 19, 17, 176, 107, 163, 241, 115, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNegative() throws {
    let source = """
negative
	"Answer whether the receiver is less than 0."
	^self < 0

"""
    // 3 .. 6
    let expected = [112, 117, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func testSin() throws {
    let source = """
sin
	"Answer with the angle in radians."

	^self asFloat sin

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testArcCos() throws {
    let source = """
arcCos
	^self asFloat arcCos

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testLog() throws {
    let source = """
log: aNumber
	"Answer the log base aNumber of the receiver."
	^self ln / aNumber ln

"""
    // 5 .. 10
    let expected = [112, 208, 16, 208, 185, 124]
    try runningSource(source, expecting: expected)
  }

  func testTan() throws {
    let source = """
tan
	"Answer with the angle in radians."

	^self asFloat tan

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testCos() throws {
    let source = """
cos
	"Answer with the angle in radians."

	^self asFloat cos

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testOdd() throws {
    let source = """
odd
	"Answer whether the receiver is an odd number."
	^self even == false

"""
    // 5 .. 9
    let expected = [112, 208, 114, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testPositive() throws {
    let source = """
positive
	"Answer whether the receiver is greater than or equal to 0."
	^self >= 0

"""
    // 3 .. 6
    let expected = [112, 117, 181, 124]
    try runningSource(source, expecting: expected)
  }

  func testGenerality() throws {
    let source = """
generality
	"Answer the number representing the ordering of the receiving in the
	generality hierarchy.  A number in this hierarchy coerces to numbers
	higher in hierarhcy (i.e., with larger generality numbers)."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReciprocal() throws {
    let source = """
reciprocal
	"Answer 1 divided by the receiver.  Create an error if the
	receiver is 0."

	self = 0
		ifTrue: [^self error: 'zero has no reciprocal']
		ifFalse: [^1 / self]

"""
    // 7 .. 18
    let expected = [112, 117, 182, 155, 112, 33, 224, 124, 118, 112, 185, 124]
    try runningSource(source, expecting: expected)
  }

  func testCoerce() throws {
    let source = """
coerce: aNumber
	"Answer with a number representing the argument, aNumber, that is
	represented the same kind of Number as is the receiver.
	Must be defined by all Number classes."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRetryCoercing() throws {
    let source = """
retry: aSymbol coercing: aNumber
	"Arithmetic represented by the symbol, aSymbol,
	could not be performed with the receiver and the argument,
	aNumber, because of the differences in representation.  Coerce either the
	receiver or the argument, depending on which has higher generality, and
	try again.  If the symbol is the equals sign, answer false if the argument is
	not a Number.  If the generalities are the same, create an error message."

	(aSymbol == #= and: [(aNumber isKindOf: Number) == false])
		ifTrue: [^false].
	self generality < aNumber generality
		ifTrue: [^(aNumber coerce: self) perform: aSymbol with: aNumber].
	self generality > aNumber generality
		ifTrue: [^self perform: aSymbol with: (self coerce: aNumber)].
	self error: 'coercion attempt failed'

"""
    // 19 .. 62
    let expected = [16, 34, 198, 157, 17, 65, 224, 114, 198, 144, 114, 152, 122, 112, 213, 17, 213, 178, 158, 17, 112, 228, 16, 17, 243, 124, 112, 213, 17, 213, 179, 158, 112, 16, 112, 17, 228, 243, 124, 112, 39, 230, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testEven() throws {
    let source = """
even
	"Answer whether the receiver is an even number."

	^self \\\\ 2 = 0

"""
    // 3 .. 8
    let expected = [112, 119, 186, 117, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- aNumber
	"Answer the difference between the receiver and aNumber."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSign() throws {
    let source = """
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0

"""
    // 3 .. 16
    let expected = [112, 117, 179, 153, 118, 124, 112, 117, 178, 153, 116, 124, 117, 124]
    try runningSource(source, expecting: expected)
  }

  func testTo() throws {
    let source = """
to: stop
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by 1."

	^Interval from: self to: stop by: 1

"""
    // 7 .. 13
    let expected = [65, 112, 16, 118, 131, 96, 124]
    try runningSource(source, expecting: expected)
  }

  func testRaisedTo() throws {
    let source = """
raisedTo: aNumber
	"Answer the receiver raised to aNumber."

	aNumber isInteger
		ifTrue: ["Do the special case of integer power"
				^self raisedToInteger: aNumber].
	aNumber = 0 ifTrue: [^1].		"Special case of exponent=0"
	aNumber = 1 ifTrue: [^self].		"Special case of exponent=1"
	^(aNumber * self ln) exp		"Otherwise raise it to the power using logarithms"

"""
    // 11 .. 34
    let expected = [16, 209, 155, 112, 16, 224, 124, 16, 117, 182, 153, 118, 124, 16, 118, 182, 152, 120, 16, 112, 211, 184, 210, 124]
    try runningSource(source, expecting: expected)
  }

  func testFloorLog() throws {
    let source = """
floorLog: radix
	"Answer the floor of the log base radix of the receiver."
	^self asFloat floorLog: radix

"""
    // 7 .. 11
    let expected = [112, 209, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testRounded() throws {
    let source = """
rounded
	"Answer the integer nearest the receiver."
	^(self + (self sign / 2)) truncated

"""
    // 7 .. 14
    let expected = [112, 112, 209, 119, 185, 176, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsInteger() throws {
    let source = """
asInteger
	"Answer an integer nearest the receiver toward zero."
	^self truncated

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testDegreesToRadians() throws {
    let source = """
degreesToRadians
	"The receiver is assumed to represent degrees.  Answer the
	conversion to radians."
	^self asFloat degreesToRadians

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
