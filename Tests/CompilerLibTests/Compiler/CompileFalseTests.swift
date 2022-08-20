import XCTest
@testable import CompilerLib

final class CompileFalseTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("False", instanceVariables: [])
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
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value

"""
    // 3 .. 5
    let expected = [16, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testIfTrueIfFalse() throws {
    let source = """
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value

"""
    // 3 .. 5
    let expected = [17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testIfFalseIfTrue() throws {
    let source = """
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value

"""
    // 3 .. 5
    let expected = [16, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testAnd() throws {
    let source = """
and: alternativeBlock
	"Nonevaluating conjunction -- answer with false since the receiver is false."

	^self

"""
    // 3 .. 3
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testOr() throws {
    let source = """
or: alternativeBlock
	"Nonevaluating disjunction -- answer value of alternativeBlock."

	^alternativeBlock value

"""
    // 3 .. 5
    let expected = [16, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	"Print false."
	aStream nextPutAll: 'false'

"""
    // 7 .. 11
    let expected = [16, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAND() throws {
    let source = """
& alternativeObject
	"Evaluating conjunction -- answer false since receiver is false."

	^self

"""
    // 3 .. 3
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testOR() throws {
    let source = """
| aBoolean
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean."

	^aBoolean

"""
    // 3 .. 4
    let expected = [16, 124]
    try runningSource(source, expecting: expected)
  }

  func testNot() throws {
    let source = """
not
	"Negation -- answer true since the receiver is false."

	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testIfTrue() throws {
    let source = """
ifTrue: alternativeBlock
	"Since the condition is false, answer the value of the false alternative,
	which is nil. Execution does not actually reach here because the
	expression is compiled in-line."

	^nil

"""
    // 3 .. 3
    let expected = [123]
    try runningSource(source, expecting: expected)
  }

}
