import XCTest
@testable import CompilerLib

final class CompileCodeInvestigationTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("CodeInvestigation", instanceVariables: ["instVar1", "instVar2"])
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
test1
  instVar1 < instVar2
    ifTrue: [ instVar1 _ instVar2 ]
"""
    compiler.context.literals = []
    let expected = [0, 1, 178, 153, 1, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func test2() throws {
    let source = """
test2
  instVar1 < instVar2
    ifTrue: [ instVar1 _ instVar2 ].
  instVar2 _ instVar1
"""
    compiler.context.literals = []
    let expected = [0, 1, 178, 153, 1, 96, 0, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func test3() throws {
    let source = """
test3
  instVar2 _ instVar1 < instVar2
    ifTrue: [ instVar1 ]
"""
    compiler.context.literals = []
    let expected = [0, 1, 178, 153, 0, 144, 115, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func test4() throws {
    let source = """
test4
  instVar2 _ instVar1 < instVar2
    ifTrue: [ instVar1 ]
    ifFalse: [ instVar2 ]
"""
    compiler.context.literals = []
    let expected = [0, 1, 178, 153, 0, 144, 1, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func test11() throws {
    let source = """
test11
  instVar1 < instVar2
    ifTrue: [ instVar1 size ]
"""
    compiler.context.literals = []
    let expected = [0, 1, 178, 154, 0, 194, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func test17() throws {
    let source = """
test17
  instVar1 < instVar2
    ifTrue: [ instVar1 _ 3 ]
    ifFalse: [ instVar2 size ]
"""
    compiler.context.literals = [
      .intConstant(3)
    ]
    let expected = [0, 1, 178, 155, 32, 129, 0, 145, 1, 194, 135, 120]
    try runningSource(source, expecting: expected)
  }
}
