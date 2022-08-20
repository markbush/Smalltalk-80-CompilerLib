import XCTest
@testable import CompilerLib

final class CompileBooleanTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Boolean", instanceVariables: [])
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


  func testEqv() throws {
    let source = """
eqv: aBoolean
	"Answer true if the receiver is equivalent to aBoolean."

	^self == aBoolean

"""
    // 3 .. 6
    let expected = [112, 16, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testAND() throws {
    let source = """
& aBoolean
	"Evaluating conjunction -- Evaluate the argument.  Then answer true if both the
	receiver and the argument are true."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOR() throws {
    let source = """
| aBoolean
	"Evaluating disjunction (OR) -- Evaluate the argument.  Then answer true if
	either the receiver or the argument is true."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIfTrue() throws {
    let source = """
ifTrue: alternativeBlock
	"If the receiver is false (i.e., the condition is false), then the value is the false
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIfFalse() throws {
    let source = """
ifFalse: alternativeBlock
	"If the receiver is true (i.e., the condition is true), then the value is the true
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIfTrueIfFalse() throws {
    let source = """
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	"If the receiver is true (i.e., the condition is true), then answer the value of the
	argument trueAlternativeBlock.  If the receiver is false, answer the result of
	evaluating the argument falseAlternativeBlock.  If the receiver is a nonBoolean
	then create an error message.  Execution does not actually reach here because the
	expression is compiled in-line."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIfFalseIfTrue() throws {
    let source = """
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
	"Same as ifTrue:ifFalse:"
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAnd() throws {
    let source = """
and: alternativeBlock
	"Nonevaluating conjunction -- if the receiver is true, answer the value of
	the argument, alternativeBlock; otherwise answer false without evaluating the
	argument."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOr() throws {
    let source = """
or: alternativeBlock
	"Nonevaluating disjunction -- if the receiver is false, answer the value of
	the argument, alternativeBlock; otherwise answer true without evaluating the
	argument."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNot() throws {
    let source = """
not
	"Negation-- answer true if the receiver is false, answer false if the receiver is true."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testXor() throws {
    let source = """
xor: aBoolean
	"Exclusive OR -- answer true if the receiver is not equivalent to aBoolean."

	^(self == aBoolean) not

"""
    // 5 .. 9
    let expected = [112, 16, 198, 208, 124]
    try runningSource(source, expecting: expected)
  }


  func testStoreOn() throws {
    let source = """
storeOn: aStream
	self printOn: aStream

"""
    // 5 .. 9
    let expected = [112, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
