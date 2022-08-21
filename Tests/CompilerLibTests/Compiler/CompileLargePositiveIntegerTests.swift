import XCTest
@testable import CompilerLib

final class CompileLargePositiveIntegerTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("LargePositiveInteger", instanceVariables: [])
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
bitXor: anInteger
	"Logical XOR the twos-complement representation of the receiver with the
	argument and return the result.  Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 36>
	^super bitXor: anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= anInteger
	"Compare the receiver with the argument and return true if the receiver is
	equal to the argument.  Otherwise return false.  Fail if the argument is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 27>
	^super = anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitAt() throws {
    let source = """
digitAt: index
	"Answer with the value of an indexable field in the receiver. Fail if the
	argument (the index) is not an Integer or is out of bounds. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 60>
	self digitLength < index
		ifTrue: [^0]
		ifFalse: [^super at: index]

"""
    // 11 .. 22
    let expected = [112, 209, 16, 178, 153, 117, 124, 112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitAnd() throws {
    let source = """
bitAnd: anInteger
	"Logical AND the twos-complement representation of the receiver with the
	argument and return the result.  Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 34>
	^super bitAnd: anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitOr() throws {
    let source = """
bitOr: anInteger
	"Logical OR the twos-complement representation of the receiver with the
	argument and return the result.  Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 35>
	^super bitOr: anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitShift() throws {
    let source = """
bitShift: anInteger
	"Answer with a Integer whose value (in twos-complement representation) is the
	receiver's value (in twos-complement representation) shifted left by the
	number of bits indicated by the argument.  Negative arguments shift right.
	Zeros are shifted in from the right in left shifts.  The sign bit is extended in right
	shifts.  Fail if the result cannot be represented as a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive."

	<primitive: 37>
	^super bitShift: anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }


  func testMultiply() throws {
    let source = """
* anInteger
	"Multiply the receiver by the argument and answer with an Integer result.  Fail if
	either the argument or the result is not a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 29>
	^super * anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitAtPut() throws {
    let source = """
digitAt: index put: value
	"Store the second argument (value) in the indexable field of the receiver
	indicated by index.  Fail if the value is negative or is larger than 255.  Fail if the
	index is not an Integer or is out of bounds.  Answer with the value that was
	stored.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	^super at: index put: value

"""
    // 9 .. 14
    let expected = [112, 16, 17, 133, 64, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsObject() throws {
    let source = """
asObject
	"This is the inverse of Object.asOop; look there for details."
	<primitive: 76>
	self < 32768
		ifTrue: [^ (self-32768) asObject].  "16K...32K-1 synonym for -16K...-1"
	self < 49152
		ifTrue: [^ self-32768].	"32K...48K-1 --> SmallIntegers 0...16K-1"
	self < 65536
		ifTrue: [^ self-65536].	"48K...64K-1 --> SmallIntegers -16K...-1"
	^ #NonExistentObject

"""
    // 17 .. 43
    let expected = [112, 33, 178, 156, 112, 33, 177, 208, 124, 112, 34, 178, 155, 112, 33, 177, 124, 112, 35, 178, 155, 112, 35, 177, 124, 36, 124]
    try runningSource(source, expecting: expected)
  }

  func testHighBit() throws {
    let source = """
highBit
	"Answer with the index of the high order bit of the binary representation of this
	number "

	^self lastDigit highBit + (8 * (self digitLength - 1))

"""
    // 11 .. 21
    let expected = [112, 209, 208, 34, 112, 211, 118, 177, 184, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testDiv() throws {
    let source = """
// anInteger
	"Divide the receiver by the argument and return the result.  Round the result
	down towards negative infinity to make it a whole integer.  Fail if the
	argument is 0.  Fail if either the argument or the result is not a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 32>
	^super // anInteger
"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testPlus() throws {
    let source = """
+ anInteger
	"Add the receiver to the argument and answer with an Integer result.  Fail if either
	the argument or the result is not a SmallInteger or a LargePositiveInteger less than
	65536.  Optional.  See Object documentation whatIsAPrimitive."

	<primitive: 21>
	^super + anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitLength() throws {
    let source = """
digitLength
	"Answer with the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< anInteger
	"Compare the receiver with the argument and return true if the receiver is less
	than the argument.  Otherwise return false.  Fail if the argument is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 23>
	^super < anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testMod() throws {
    let source = """
\\\\ anInteger
	"Take the receiver modulo the argument.  The result is the remainder rounded
	towards negative infinity, of the receiver divided by the argument.  Fail if the
	argument is 0.  Fail if either the argument or the result is not a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive."

	<primitive: 31>
	^super \\\\ anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= anInteger
	"Compare the receiver with the argument and return true if the receiver is less
	than or equal to the argument.  Otherwise return false.  Fail if the argument is
	not a SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See
	Object documentation whatIsAPrimitive."

	<primitive: 25>
	^super <= anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testNegative() throws {
    let source = """
negative
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= anInteger
	"Compare the receiver with the argument and answer with true if the receiver
	is greater than or equal to the argument.  Otherwise return false.  Fail if the
	argument is not a SmallInteger or a LargePositiveInteger less than 65536.
	Optional.  See Object documentation whatIsAPrimitive."

	<primitive: 26>
	^super >= anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testPositive() throws {
    let source = """
positive
	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> anInteger
	"Compare the receiver with the argument and return true if the receiver is
	greater than the argument.  Otherwise return false.  Fail if the argument is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 24>
	^super > anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testNotEquals() throws {
    let source = """
~= anInteger
	"Compare the receiver with the argument and answer true if the receiver is not
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a
	SmallInteger or a LargePositiveInteger less than 65536.  Optional.  See Object
	documentation whatIsAPrimitive."

	<primitive: 28>
	^super ~= anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ anInteger
	"Divide the receiver by the argument and answer with the result if the division
	is exact.  Fail if the result is not a whole integer.  Fail if the argument is 0.  Fail if
	either the argument or the result is not a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 30>
	^super / anInteger
"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- anInteger
	"Subtract the argument from the receiver and answer with an Integer result.  Fail if
	either the argument or the result is not a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 22>
	^super - anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testQuo() throws {
    let source = """
quo: anInteger
	"Divide the receiver by the argument and return the result.  Round the result
	down towards zero to make it a whole integer.  Fail if the argument is 0.  Fail if
	either the argument or the result is not a SmallInteger or a
	LargePositiveInteger less than 65536.  Optional.  See Object documentation
	whatIsAPrimitive."

	<primitive: 33>
	^super quo: anInteger

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testTruncated() throws {
    let source = """
truncated
	"If I can be represented as a SmallInteger, return the
	converted result.  This message should really be called
	something else, since it is only used internally after
	arithmetic and bit operations, and in a few external
	places that construct LargePositiveIntegers in non-
	standard ways."

	| size partial maxSize |
	size _ self digitLength.
	size = 0 ifTrue: [^0].
	partial _ self digitAt: size.
	partial = 0 ifTrue: [^(self growby: -1) truncated].
	maxSize _ SmallInteger maxBytes.
	(size < maxSize or: [size = maxSize and: [partial <= MaxHi]])
		ifTrue:
			["Convert back to a SmallInteger."
			[(size _ size - 1) > 0]
				whileTrue:
					[partial _ (partial bitShift: 8) + (self digitAt: size)].  "Can't overflow"
			^partial]

"""
    // 19 .. 83
    let expected = [112, 208, 104, 16, 117, 182, 153, 117, 124, 112, 16, 225, 105, 17, 117, 182, 156, 112, 116, 227, 210, 124, 69, 212, 106, 16, 18, 178, 154, 113, 164, 9, 16, 18, 182, 155, 17, 71, 180, 144, 114, 172, 21, 16, 118, 177, 129, 64, 117, 179, 172, 10, 17, 38, 188, 112, 16, 225, 176, 105, 163, 237, 17, 124, 120]
    try runningSource(source, expecting: expected)
  }

  func testNegated() throws {
    let source = """
negated
	| minVal |
	((self digitAt: 1) = 0 and: [self digitLength = 1]) ifTrue: [^self].
	"Zero"
	minVal _ SmallInteger minVal.
	((self digitAt: 2) = (0 - (minVal // 256)) and: [(self digitAt: 1) = 0])
		ifTrue: [^minVal].
	^self copyto: (LargeNegativeInteger new: self digitLength)

"""
    // 17 .. 60
    let expected = [112, 118, 225, 117, 182, 156, 112, 208, 118, 182, 144, 114, 152, 120, 67, 210, 104, 112, 119, 225, 117, 16, 36, 189, 177, 182, 157, 112, 118, 225, 117, 182, 144, 114, 153, 16, 124, 112, 70, 112, 208, 205, 229, 124]
    try runningSource(source, expecting: expected)
  }

}
