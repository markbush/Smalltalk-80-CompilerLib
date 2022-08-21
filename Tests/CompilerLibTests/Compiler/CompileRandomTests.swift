import XCTest
@testable import CompilerLib

final class CompileRandomTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Random", instanceVariables: ["seed"])
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

  func testContents() throws {
    let source = """
contents
	^self shouldNotImplement

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetSeed() throws {
    let source = """
setSeed
	seed _ Time millisecondClockValue bitAnd: 65535
		"Time millisecondClockValue gives a large integer;  I only want the lower 16 bits."

"""
    // 9 .. 14
    let expected = [65, 208, 34, 190, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtEnd() throws {
    let source = """
atEnd
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testNext() throws {
    let source = """
next
	"Answer with the next random number."

	| temp |
	[seed _ 13849 + (27181 * seed) bitAnd: 65535.
	0 = (temp _ seed / 65536.0)] whileTrue.
	^temp

"""
    // 13 .. 37
    let expected = [137, 117, 200, 164, 16, 33, 34, 0, 184, 176, 35, 190, 96, 117, 0, 36, 185, 129, 64, 182, 125, 208, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testNextPut() throws {
    let source = """
nextPut: anObject
	^self shouldNotImplement

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
