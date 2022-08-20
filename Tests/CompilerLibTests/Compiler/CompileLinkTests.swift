import XCTest
@testable import CompilerLib

final class CompileLinkTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Link", instanceVariables: ["nextLink"])
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

  func testNextLink() throws {
    let source = """
nextLink: aLink
	"Store the argument, as the Link to which the receiver refers."
	^nextLink _ aLink
"""
    // 3 .. 6
    let expected = [16, 129, 0, 124]
    try runningSource(source, expecting: expected)
  }
}
