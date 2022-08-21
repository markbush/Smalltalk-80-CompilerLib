import XCTest
@testable import CompilerLib

final class CompileLargeNegativeIntegerTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("LargeNegativeInteger", instanceVariables: [])
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

  func testDigitAtPut() throws {
    let source = """
digitAt: index put: value
	"Store the second argument (value) in the indexable field of the receiver
	indicated by index.  Fail if the value is negative or is larger than 255.  Fail if
	the index is not an Integer or is out of bounds.  Answer the value that was
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
	"This is the inverse of Object.asOop; look there for details.
	Only Stretch should encounter this code."
	self >= -16384
		ifTrue:
			[self even
				ifTrue: [^ self // 2]         "even --> negative"
				ifFalse: [^ (self+1) // -2]]  "odd --> positive"
		ifFalse:
			[^ #NonExistentObject]

"""
    // 11 .. 30
    let expected = [112, 35, 181, 172, 13, 112, 210, 155, 112, 119, 189, 124, 112, 118, 176, 33, 189, 124, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testHighBit() throws {
    let source = """
highBit
	self error: 'highBit not defined for negative numbers'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testDigitLength() throws {
    let source = """
digitLength
	"Answer the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testDigitAt() throws {
    let source = """
digitAt: index
	"Answer the value of an indexable field in the receiver.  Fail if the argument
	(the index) is not an Integer or is out of bounds.  Essential.  See Object
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

  func testNegative() throws {
    let source = """
negative
	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testPositive() throws {
    let source = """
positive
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testTruncated() throws {
    let source = """
truncated
	"If I can be represented as a SmallInteger, return the
	converted result.  This message should really be called
	something else, since it is only used internally after
	arithmetic and bit operations."

	| size partial maxSize |
	size _ self digitLength.
	size = 0 ifTrue: [^0].
	partial _ self digitAt: size.
	partial = 0 ifTrue: [^(self growby: -1) truncated].
	maxSize _ SmallInteger maxBytes.
	(size < maxSize or: [size = maxSize and: [partial < MinHi or: [partial = MinHi and: [(self anyBitTo: maxSize - 1 * 8) not]]]])
		ifTrue:
			["Convert back to a SmallInteger."
			partial _ 0 - partial.
			[(size _ size - 1) > 0]
				whileTrue:
					[partial _ (partial bitShift: 8) - (self digitAt: size)].  "Can't overflow"
			^partial]

"""
    // 23 .. 111
    let expected = [112, 208, 104, 16, 117, 182, 153, 117, 124, 112, 16, 225, 105, 17, 117, 182, 156, 112, 116, 227, 210, 124, 69, 212, 106, 16, 18, 178, 154, 113, 164, 29, 16, 18, 182, 172, 23, 17, 73, 178, 154, 113, 164, 15, 17, 73, 182, 172, 9, 112, 18, 118, 177, 38, 184, 232, 215, 144, 114, 144, 114, 172, 25, 117, 17, 177, 105, 16, 118, 177, 129, 64, 117, 179, 172, 10, 17, 38, 188, 112, 16, 225, 177, 105, 163, 237, 17, 124, 120]
    try runningSource(source, expecting: expected)
  }

  func testNegated() throws {
    let source = """
negated
	^self copyto: (LargePositiveInteger new: self digitLength)

"""
    // 9 .. 15
    let expected = [112, 65, 112, 210, 205, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOnBase() throws {
    let source = """
printOn: aStream base: b
	aStream nextPut: $-.
	super printOn: aStream base: b

"""
    // 9 .. 19
    let expected = [16, 32, 196, 135, 112, 16, 17, 133, 65, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAbs() throws {
    let source = """
abs
	^self negated

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
