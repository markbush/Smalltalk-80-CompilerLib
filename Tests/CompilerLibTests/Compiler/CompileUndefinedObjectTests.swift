import XCTest
@testable import CompilerLib

final class CompileUndefinedObjectTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("UndefinedObject", instanceVariables: [])
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

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	aStream nextPutAll: 'nil'

"""
    // 7 .. 11
    let expected = [16, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPutAll: 'nil'

"""
    // 7 .. 11
    let expected = [16, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }



  func testAddDependent() throws {
    let source = """
addDependent: ignored
	self error: 'Nil should not have dependents'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsNil() throws {
    let source = """
isNil
	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testNotNil() throws {
    let source = """
notNil
	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

}
