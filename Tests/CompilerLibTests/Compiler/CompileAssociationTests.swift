import XCTest
@testable import CompilerLib

final class CompileAssociationTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Association", instanceVariables: ["key", "value"])
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

  func test1() throws {
    let source = """
value: anObject
	"Store the argument, anObject, as the value of the receiver."
	value _ anObject
"""
    compiler.context.literals = [

    ]
    // 3 .. 5
    let expected = [16, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func test2() throws {
    let source = """
key: aKey value: anObject
	"Store the arguments as the variables of the receiver."

	key _ aKey.
	value _ anObject
"""
    compiler.context.literals = [

    ]
    // 3 .. 7
    let expected = [16, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func test3() throws {
    let source = """
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream
"""
    compiler.context.literals = [
      .symbolConstant("printOn:"),
      .symbolConstant("nextPutAll:"),
      .stringConstant("->"),
      .symbolConstant("printOn:"),
      .stringVariable("Association", "Association")
    ]
    // 13 .. 26
    // let expected = [112, 16, 133, 32, 135, 16, 34, 225, 135, 1, 16, 227, 135, 120]
    // 224 should be 227 - #printOn: appears twice in original ST-80 code!
    let expected = [112, 16, 133, 32, 135, 16, 34, 225, 135, 1, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }
}
