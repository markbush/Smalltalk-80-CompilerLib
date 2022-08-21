import XCTest
@testable import CompilerLib

final class CompileReadStreamTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("ReadStream", instanceVariables: ["collection", "position", "readLimit"])
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

  func testNext() throws {
    let source = """
next
	"Answer with the next object in the Stream represented by the receiver.
	Fail if the collection of this stream is not an Array or a String. Fail if
	the stream is positioned at its end, or if the position is out of bounds in
	the collection. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 65>
	position >= readLimit
		ifTrue: [^nil]
		ifFalse: [^collection at: (position _ position + 1)]

"""
    // 7 .. 19
    let expected = [1, 2, 181, 152, 123, 0, 1, 118, 176, 129, 1, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testOnFromTo() throws {
    let source = """
on: aCollection from: firstIndex to: lastIndex
	| len |
	collection _ aCollection.
	readLimit _  lastIndex > (len _ collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position _ firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]

"""
    // 3 .. 26
    let expected = [16, 96, 18, 0, 194, 129, 67, 179, 153, 19, 144, 18, 98, 17, 118, 180, 153, 117, 146, 17, 118, 177, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextPut() throws {
    let source = """
nextPut: anObject
	self shouldNotImplement

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
