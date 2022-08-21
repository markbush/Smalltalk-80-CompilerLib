import XCTest
@testable import CompilerLib

final class CompileMagnitudeTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Magnitude", instanceVariables: [])
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

  func testGreaterThan() throws {
    let source = """
> aMagnitude
	"Compare the receiver with the argument and answer with true if the
	receiver is greater than the argument. Otherwise answer false."

	^aMagnitude < self

"""
    // 3 .. 6
    let expected = [16, 112, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func testMin() throws {
    let source = """
min: aMagnitude
	"Answer the receiver or the argument, whichever has the lesser magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]

"""
    // 3 .. 9
    let expected = [112, 16, 178, 152, 120, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testMax() throws {
    let source = """
max: aMagnitude
	"Answer the receiver or the argument, whichever has the greater magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]

"""
    // 3 .. 9
    let expected = [112, 16, 179, 152, 120, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility

"""
    // 5 .. 7
    let expected = [112, 208, 124]
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

  func testBetweenAnd() throws {
    let source = """
between: min and: max
	"Answer whether the receiver is less than or equal to the argument, max,
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]

"""
    // 3 .. 12
    let expected = [112, 16, 181, 155, 112, 17, 180, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= aMagnitude
	"Compare the receiver with the argument and answer with true if the
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aMagnitude
	"Compare the receiver with the argument and answer with true if the
	receiver is less than the argument. Otherwise answer false."

	^self subclassResponsibility

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= aMagnitude
	"Compare the receiver with the argument and answer with true if the
	receiver is less than or equal to the argument. Otherwise answer false."

	^(self > aMagnitude) not

"""
    // 5 .. 9
    let expected = [112, 16, 179, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= aMagnitude
	"Compare the receiver with the argument and answer with true if the
	receiver is greater than or equal to the argument. Otherwise answer false."

	^(self < aMagnitude) not

"""
    // 5 .. 9
    let expected = [112, 16, 178, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
