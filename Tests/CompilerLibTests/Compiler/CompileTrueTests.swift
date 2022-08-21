import XCTest
@testable import CompilerLib

final class CompileTrueTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("True", instanceVariables: [])
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

  func testIfFalse() throws {
    let source = """
ifFalse: alternativeBlock
	"Since the condition is true, the value is the true alternative, which is nil.
	Execution does not actually reach here because the expression is compiled
	in-line."

	^nil

"""
    // 3 .. 3
    let expected = [123]
    try runningSource(source, expecting: expected)
  }

  func testIfTrueIfFalse() throws {
    let source = """
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	"Answer with the value of trueAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value

"""
    // 3 .. 5
    let expected = [16, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testIfFalseIfTrue() throws {
    let source = """
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
	"Answer the value of trueAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value

"""
    // 3 .. 5
    let expected = [17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testAnd() throws {
    let source = """
and: alternativeBlock
	"Nonevaluating conjunction -- answer the value of alternativeBlock since
	the receiver is true."

	^alternativeBlock value

"""
    // 3 .. 5
    let expected = [16, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testOr() throws {
    let source = """
or: alternativeBlock
	"Nonevaluating disjunction -- answer true since the receiver is true."

	^self

"""
    // 3 .. 3
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPutAll: 'true'

"""
    // 7 .. 11
    let expected = [16, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAND() throws {
    let source = """
& alternativeObject
	"Evaluating conjunction -- answer alternativeObject since receiver is true."

	^alternativeObject

"""
    // 3 .. 4
    let expected = [16, 124]
    try runningSource(source, expecting: expected)
  }

  func testOR() throws {
    let source = """
| aBoolean
	"Evaluating disjunction (OR) -- answer true since the receiver is true."

	^self

"""
    // 3 .. 3
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testNot() throws {
    let source = """
not
	"Negation--answer false since the receiver is true."

	^false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testIfTrue() throws {
    let source = """
ifTrue: alternativeBlock
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value

"""
    // 3 .. 5
    let expected = [16, 201, 124]
    try runningSource(source, expecting: expected)
  }

}
