import XCTest
@testable import CompilerLib

final class CompileSmallIntegerTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("SmallInteger", instanceVariables: [])
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

  func testBitXor() throws {
    let source = """
bitXor: arg
	"Exclusive OR the twos-complement representation of the receiver with the
	argument and answer with the result.  Fail if the argument is not a
	SmallInteger.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 16>
	^arg bitXor: self

"""
    // 9 .. 12
    let expected = [16, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testHashMappedBy() throws {
    let source = """
hashMappedBy: map
	"My hash is independent of my oop"
	^ self hash

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsFloat() throws {
    let source = """
asFloat
	"Create and answer an instance of Float whose value is the value of the receiver.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 40>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testInstVarAt() throws {
    let source = """
instVarAt: i
	"small integer has to be specially handled"

	i = 1 ifTrue: [^self].
	self error: 'argument too big for small integer instVarAt:'

"""
    // 7 .. 16
    let expected = [16, 118, 182, 152, 120, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testQuo() throws {
    let source = """
quo: aNumber
	"Divide the receiver by the argument and answer with the result.  Round the
	result down towards zero to make it a whole integer.  Fail if the argument is 0 or is
	not a SmallInteger.  Optional.  See Object documentation whatIsAPrimitive."

	<primitive: 13>
	aNumber = 0 ifTrue: [^self error: 'Attempt to divide by zero'].
	(aNumber isMemberOf: SmallInteger)
		ifTrue: [self primitiveFailed]
		ifFalse: [^super quo: aNumber]

"""
    // 19 .. 40
    let expected = [16, 117, 182, 155, 112, 33, 224, 124, 16, 69, 228, 154, 112, 211, 148, 112, 16, 133, 34, 124, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aNumber
	"Compare the receiver with the argument and answer with true if the receiver is less
	than the argument.  Otherwise answer false.  Fail if the argument is not a
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 3>
	^super < aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= aNumber
	"Compare the receiver with the argument and answer true if the receiver is less
	than or equal to the argument.  Otherwise answer false.  Fail if the argument is
	not a SmallInteger.  Optional.  No Lookup.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 5>
	^super <= aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= aNumber
	"Compare the receiver with the argument and answer true if the receiver is
	greater than or equal to the argument.  Otherwise answer false.  Fail if the
	argument is not a SmallInteger.  Optional.  No Lookup.  See Object
	documentation whatIsAPrimitive."

	<primitive: 6>
	^super >= aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> aNumber
	"Compare the receiver with the argument and answer true if the receiver is
	greater than the argument.  Otherwise answer false.  Fail if the argument is not a
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 4>
	^super > aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testNotEquals() throws {
    let source = """
~= aNumber
	"Compare the receiver with the argument and answer true if the receiver is not
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a
	SmallInteger.  Essential.  No Lookup.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 8>
	^super ~= aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }


  func testPrintOnBase() throws {
    let source = """
printOn: aStream base: b
	| i x |
	(x _ self) < 0
		ifTrue:
			[aStream nextPut: $-.
			x _ self negated].
	i _ 0.
	[x >= b]
		whileTrue:
			[Digitbuffer at: (i _ i + 1) put: x \\\\ b.
			x _ x // b].
	Digitbuffer at: (i _ i + 1) put: x.
	[i > 0]
		whileTrue:
			[aStream nextPut: (Character digitValue: (Digitbuffer at: i)).
			i _ i - 1]

"""
    // 13 .. 78
    let expected = [112, 129, 67, 117, 178, 158, 16, 32, 196, 135, 112, 209, 107, 117, 106, 19, 17, 181, 172, 17, 66, 18, 118, 176, 129, 66, 19, 17, 186, 193, 135, 19, 17, 189, 107, 163, 234, 66, 18, 118, 176, 129, 66, 19, 193, 135, 18, 117, 179, 172, 14, 16, 68, 66, 18, 192, 227, 196, 135, 18, 118, 177, 106, 163, 237, 120]
    try runningSource(source, expecting: expected)
  }

  func testBitAnd() throws {
    let source = """
bitAnd: arg
	"Logical AND the twos-complement representation of the receiver with the
	argument and answer with the result.  Fail if the argument is not a SmallInteger.
	Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 14>
	^arg bitAnd: self

"""
    // 7 .. 10
    let expected = [16, 112, 190, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitOr() throws {
    let source = """
bitOr: arg
	"Logical OR the twos-complement representation of the receiver with the
	argument and answer with the result.  Fail if the argument is not a SmallInteger.
	Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 15>
	^arg bitOr: self

"""
    // 7 .. 10
    let expected = [16, 112, 191, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitShift() throws {
    let source = """
bitShift: arg
	"Answers with a SmallInteger whose value (in twos-complement
	representation) is the receiver's value (in twos-complement representation)
	shifted left by the number of bits indicated by the argument.  Negative
	arguments shift right.  Zeros are shifted in from the right in left shifts.  The sign
	bit is extended in right shifts.  Fail if the result cannot be represented as a
	SmallInteger.  Essential.  No Lookup.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 17>
	^super bitShift: arg

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= aNumber
	"Compare the receiver with the argument and answer true if the receiver is
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a
	SmallInteger.  Essential.  No Lookup.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 7>
	^super = aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubtractOrFail() throws {
    let source = """
subtractOrFail: aNumber
	"This is a private copy of the subtraction primitive,
	used by SmallInteger class initialize to discover the
	correct value of SmallInteger minVal."

	<primitive: 2>
	^nil

"""
    // 7 .. 7
    let expected = [123]
    try runningSource(source, expecting: expected)
  }


  func testMultiply() throws {
    let source = """
* aNumber
	"Multiply the receiver by the argument and answer with the result if it is a
	SmallInterger.  Fail if the argument or the result is not a SmallInteger.
	Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 9>
	self = 0 ifTrue: [^0].
	"This eliminates the need for a self=0 check in LargeInteger *"
	^super * aNumber

"""
    // 9 .. 19
    let expected = [112, 117, 182, 153, 117, 124, 112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testMod() throws {
    let source = """
\\\\ aNumber
	"Take the receiver modulo the argument.  The result is the remainder rounded
	towards negative infinity, of the receiver divided by the argument.  Fail if the
	argument is 0 or is not a SmallInteger.  Optional.  No Lookup.  See Object
	documentation whatIsAPrimitive. "

	<primitive: 11>
	^super \\\\ aNumber"Do with // if primitive fails"

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsOop() throws {
    let source = """
asOop
	"Answer an integer which is unique to me, consistent with Object.asOop.
	See Object.asOop for detailed documentation"
	self >= 0
		ifTrue: [^ 32768 + self]		"0...16K-1 --> 32K...48K-1"
		ifFalse: [^ 65536 + self]		"-16k...-1 --> 48K...64K-1"

"""
    // 7 .. 18
    let expected = [112, 117, 181, 155, 33, 112, 176, 124, 32, 112, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ aNumber
	"This primitive (for /) divides the receiver by the argument and returns the
	result if the division is exact.  Fail if the result is not a whole integer.  Fail if the
	argument is 0 or is not a SmallInteger.  Optional.  No Lookup.  See Object
	documentation whatIsAPrimitive."

	<primitive: 10>
	aNumber = 0 ifTrue: [^self error: 'division by 0'].
	(aNumber isMemberOf: SmallInteger)
		ifTrue: [^(Fraction numerator: self denominator: aNumber) reduced]
		ifFalse: [^super / aNumber]
"""
    // 23 .. 45
    let expected = [16, 117, 182, 155, 112, 33, 224, 124, 16, 71, 230, 157, 69, 112, 16, 244, 211, 124, 112, 16, 133, 34, 124]
    try runningSource(source, expecting: expected)
  }

  func testDiv() throws {
    let source = """
// aNumber
	"Divide the receiver by the argument and answer with the result.  Round the result
	down towards negative infinity to make it a whole integer.  Fail if the
	argument is 0 or is not a SmallInteger.  Essential.  No Lookup.  See Object
	documentation whatIsAPrimitive. "

	<primitive: 12>
	^super // aNumber"Do with quo: if primitive fails"
"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testPlus() throws {
    let source = """
+ aNumber
	"Add the receiver to the argument and answer with the result if it is a SmallInterger.
	Fail if the argument or the result is not a SmallInteger.  Essential.  No Lookup.
	See Object documentation whatIsAPrimitive."

	<primitive: 1>
	^super + aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testFromStringRadix() throws {
    let source = """
fromString: str radix: radix
	| maxdigit c val |
	maxdigit _
		radix + (radix > 10
					ifTrue: [55 - 1]
					ifFalse: [48 - 1]).
	val _ 0.
	1 to: str size do:
		[:i |
		c _ str at: i.
		(c < 48 ifFalse: [c > maxdigit])
			ifTrue: [^false].
		val _ val * radix + (c <= 57
							ifTrue: [c - 48]
							ifFalse:
								[c < 65 ifTrue: [^false].
								c - 55])].
	^val

"""
    // 15 .. 81
    let expected = [17, 17, 34, 179, 155, 33, 118, 177, 146, 32, 118, 177, 176, 106, 117, 108, 118, 16, 194, 137, 118, 200, 164, 39, 109, 16, 21, 192, 107, 19, 32, 178, 153, 115, 146, 19, 18, 179, 152, 122, 20, 17, 184, 19, 37, 180, 155, 19, 32, 177, 151, 19, 36, 178, 152, 122, 19, 33, 177, 176, 129, 68, 125, 243, 135, 20, 124]
    try runningSource(source, expecting: expected)
  }

  func testOdd() throws {
    let source = """
odd
	^(self bitAnd: 1) = 1

"""
    // 3 .. 8
    let expected = [112, 118, 190, 118, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testGenerality() throws {
    let source = """
generality
	^20

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }


  func testCoerce() throws {
    let source = """
coerce: n
	^n truncated

"""
    // 5 .. 7
    let expected = [16, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testEven() throws {
    let source = """
even
	^(self bitAnd: 1) = 0

"""
    // 3 .. 8
    let expected = [112, 118, 190, 117, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- aNumber
	"Subtract the argument from the receiver and answer with the result if it is a
	SmallInterger.  Fail if the argument or the result is not a SmallInteger.
	Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 2>
	^super - aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitAtPut() throws {
    let source = """
digitAt: n put: value
	"Fails. The digits of a small integer can not be modified."

	self error: 'You cant store in a SmallInteger'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsObject() throws {
    let source = """
asObject
	"This is the inverse of Object.asOop; look there for details.
	See also Object documentation whatIsAPrimitive."
	<primitive: 76>
	^self primitiveFailed

"""
    // 9 .. 11
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testHighBit() throws {
    let source = """
highBit
	| i bit |
	self < 0 ifTrue: [^self error: 'highBit is not defined for negative numbers'].
	self = 0 ifTrue: [^0].
	i _ 1.
	bit _ 1.
	[self > bit]
		whileTrue:
			[i _ i + 1.
			bit _ bit + bit + 1].
	^i

"""
    // 7 .. 43
    let expected = [112, 117, 178, 155, 112, 33, 224, 124, 112, 117, 182, 153, 117, 124, 118, 104, 118, 105, 112, 17, 179, 172, 12, 16, 118, 176, 104, 17, 17, 176, 118, 176, 105, 163, 239, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitLength() throws {
    let source = """
digitLength
	"Answer with the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Included so that a SmallInteger can behave
	like a LargeInteger."

	| maxSize minValue size |
	(self < 16r100 and: [self > -16r100]) ifTrue: [^1].
	maxSize _ SmallInteger maxBytes.
	maxSize = 2 ifTrue: [^2].  "Make things go fast for 16-bit systems"
	minValue _ -16r100.
	size _ 2.
	[size < maxSize]
		whileTrue:
			[minValue _ minValue bitShift: 8.
			(self <= (-1 - minValue) and: [self > minValue]) ifTrue: [^size].
			size _ size + 1].
	^maxSize

"""
    // 13 .. 68
    let expected = [112, 33, 178, 155, 112, 32, 179, 144, 114, 153, 118, 124, 67, 210, 104, 16, 119, 182, 153, 119, 124, 32, 105, 119, 106, 18, 16, 178, 172, 24, 17, 36, 188, 105, 112, 116, 17, 177, 180, 155, 112, 17, 179, 144, 114, 153, 18, 124, 18, 118, 176, 106, 163, 227, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitAt() throws {
    let source = """
digitAt: n
	"Answer the value of an apparent indexable field.
	This is provided for compatibility with LargeInteger."

	 n = 1
		ifTrue:
			["Negate carefully in case I am SmallInteger minVal"
			self < 0
				ifTrue: [^-256 - self bitAnd: 255].
			^self bitAnd: 255]
		ifFalse:
			[self < 0
				ifTrue: [^(-256 - self bitShift: -8) + 1 digitAt: n - 1].
			^(self bitShift: 8 - (n bitShift: 3)) bitAnd: 255]

"""
    // 15 .. 60
    let expected = [16, 118, 182, 172, 14, 112, 117, 178, 157, 33, 112, 177, 37, 190, 124, 112, 37, 190, 124, 112, 117, 178, 172, 12, 33, 112, 177, 34, 188, 118, 176, 16, 118, 177, 224, 124, 112, 35, 16, 36, 188, 177, 188, 37, 190, 124]
    try runningSource(source, expecting: expected)
  }

}
