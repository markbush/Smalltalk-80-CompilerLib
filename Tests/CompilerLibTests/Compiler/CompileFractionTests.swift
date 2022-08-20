import XCTest
@testable import CompilerLib

final class CompileFractionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Fraction", instanceVariables: ["numerator", "denominator"])
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

  func testPlus() throws {
    let source = """
+ aFraction
	| commonDenominator newNumerator |
	(aFraction isMemberOf: Fraction)
		ifTrue:
			[denominator = aFraction denominator
				ifTrue: [^(Fraction
							numerator: numerator + aFraction numerator
							denominator: denominator) reduced].
			commonDenominator _ denominator lcm: aFraction denominator.
			newNumerator _ numerator
								* (commonDenominator / denominator)
								+ (aFraction numerator *
									(commonDenominator / aFraction denominator)).
			^(Fraction
				numerator: newNumerator
				denominator: commonDenominator) reduced]
		ifFalse: [^self retry: #+ coercing: aFraction]

"""
    // 21 .. 70
    let expected = [16, 68, 232, 172, 40, 1, 16, 214, 182, 172, 9, 68, 0, 16, 213, 176, 1, 243, 210, 124, 1, 16, 214, 231, 105, 0, 17, 1, 185, 184, 16, 213, 17, 16, 214, 185, 184, 176, 106, 68, 18, 17, 243, 210, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testTruncated() throws {
    let source = """
truncated
	^numerator quo: denominator

"""
    // 5 .. 8
    let expected = [0, 1, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetNumeratorDenominator() throws {
    let source = """
setNumerator: n denominator: d
	d = 0
		ifTrue: [self error: 'denominator cannot be zero']
		ifFalse:
			[numerator _ n truncated.
			denominator _ d truncated abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator _ numerator negated]]

"""
    // 13 .. 40
    let expected = [17, 117, 182, 156, 112, 36, 227, 164, 17, 16, 208, 96, 17, 208, 209, 97, 17, 117, 178, 156, 0, 210, 129, 0, 144, 115, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNegated() throws {
    let source = """
negated
	^Fraction numerator: numerator negated denominator: denominator

"""
    // 9 .. 14
    let expected = [65, 0, 210, 1, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testGenerality() throws {
    let source = """
generality
	^60

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testReciprocal() throws {
    let source = """
reciprocal
	numerator = 0 ifTrue: [self error: '0 has no reciprocal'].
	numerator = 1 ifTrue: [^denominator].
	numerator = -1 ifTrue: [^denominator negated].
	^Fraction numerator: denominator denominator: numerator

"""
    // 13 .. 38
    let expected = [0, 117, 182, 155, 112, 33, 224, 135, 0, 118, 182, 153, 1, 124, 0, 116, 182, 154, 1, 210, 124, 68, 1, 0, 243, 124]
    try runningSource(source, expecting: expected)
  }

  func testCoerce() throws {
    let source = """
coerce: aNumber
	^aNumber asFraction

"""
    // 5 .. 7
    let expected = [16, 208, 124]
    try runningSource(source, expecting: expected)
  }



  func testReduced() throws {
    let source = """
reduced
	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd _ numerator gcd: denominator.
	numer _ numerator // gcd.
	denom _ denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom

"""
    // 9 .. 37
    let expected = [0, 117, 182, 153, 117, 124, 0, 1, 224, 104, 0, 16, 189, 105, 1, 16, 189, 106, 18, 118, 182, 153, 17, 124, 66, 17, 18, 241, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- aFraction
	(aFraction isMemberOf: Fraction)
		ifTrue: [^self + aFraction negated]
		ifFalse: [^self retry: #- coercing: aFraction]

"""
    // 13 .. 26
    let expected = [16, 68, 227, 156, 112, 16, 210, 176, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }


  func testAsFloat() throws {
    let source = """
asFloat
	"Answer with a new Float that represents the same value as does the receiver."
	^numerator asFloat / denominator asFloat

"""
    // 5 .. 10
    let expected = [0, 208, 1, 208, 185, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPut: $(.
	numerator printOn: aStream.
	aStream nextPut: $/.
	denominator printOn: aStream.
	aStream nextPut: $)

"""
    // 11 .. 31
    let expected = [16, 32, 196, 135, 0, 16, 225, 135, 16, 34, 196, 135, 1, 16, 225, 135, 16, 35, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Hash is reimplemented because = is implemented."

	^numerator bitXor: denominator

"""
    // 5 .. 8
    let expected = [0, 1, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aFraction
	(aFraction isMemberOf: Fraction)
		ifTrue: [aFraction numerator = 0
				ifTrue: [^numerator < 0]
				ifFalse: [^self - aFraction < 0]]
		ifFalse: [^self retry: #< coercing: aFraction]

"""
    // 13 .. 37
    let expected = [16, 68, 227, 172, 15, 16, 210, 117, 182, 155, 0, 117, 178, 124, 112, 16, 177, 117, 178, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= aFraction
	(aFraction isMemberOf: Fraction)
		ifTrue: [aFraction numerator = 0
				ifTrue: [^numerator = 0]
				ifFalse: [^aFraction numerator = numerator
							and: [aFraction denominator = denominator]]]
		ifFalse: [^self retry: #= coercing: aFraction]

"""
    // 15 .. 45
    let expected = [16, 69, 228, 172, 21, 16, 211, 117, 182, 155, 0, 117, 182, 124, 16, 211, 0, 182, 156, 16, 210, 1, 182, 144, 114, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testMultiply() throws {
    let source = """
* aFraction
	(aFraction isMemberOf: Fraction)
		ifTrue: [^(Fraction
					numerator: numerator * aFraction numerator
					denominator: denominator * aFraction denominator)
					reduced]
		ifFalse: [^self retry: #* coercing: aFraction]

"""
    // 19 .. 40
    let expected = [16, 68, 231, 172, 12, 68, 0, 16, 213, 184, 1, 16, 214, 184, 243, 210, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ aFraction
	(aFraction isMemberOf: Fraction)
		ifTrue: [^self * aFraction reciprocal]
		ifFalse: [^self retry: #/ coercing: aFraction]
"""
    // 13 .. 26
    let expected = [16, 68, 227, 156, 112, 16, 210, 184, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

}
