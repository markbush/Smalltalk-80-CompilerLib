import XCTest
@testable import CompilerLib

final class CompileIdentitySetTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("IdentitySet", instanceVariables: ["tally"])
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

  func testFindElementOrNil() throws {
    let source = """
findElementOrNil: anObject
	"Copied from Set with equality check changed to identity"
	| index length probe pass |
	length _ self basicSize.
	pass _ 1.
	index _ anObject hash \\\\ length + 1.
	[(probe _ self basicAt: index) == nil or: [probe == anObject]]
		whileFalse: [(index _ index + 1) > length
				ifTrue:
					[index _ 1.
					pass _ pass + 1.
					pass > 2 ifTrue: [^self grow findElementOrNil: anObject]]].
	^index

"""
    // 13 .. 67
    let expected = [112, 208, 106, 118, 108, 16, 209, 18, 186, 118, 176, 105, 112, 17, 228, 129, 67, 115, 198, 153, 113, 146, 19, 16, 198, 168, 26, 17, 118, 176, 129, 65, 18, 179, 172, 15, 118, 105, 20, 118, 176, 108, 20, 119, 179, 156, 112, 211, 16, 226, 124, 163, 215, 17, 124]
    try runningSource(source, expecting: expected)
  }

}
