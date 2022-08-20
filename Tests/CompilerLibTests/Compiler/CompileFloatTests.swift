import XCTest
@testable import CompilerLib

final class CompileFloatTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Float", instanceVariables: [])
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

  func testMantissa() throws {
    let source = """
mantissa: nBits
	"Answer as an integer the most significant nBits of the mantissa of the receiver."

	^(self abs timesTwoPower: nBits-self exponent-1) truncated

"""
    // 11 .. 21
    let expected = [112, 210, 16, 112, 211, 177, 118, 177, 225, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testFloorLog() throws {
    let source = """
floorLog: radix
	"quick computation of (self log: radix) floor"

	| x |
	self < radix ifTrue: [^0]. 	"self assumed positive"
	self < radix squared ifTrue: [^1].
	x _ 2 * (self floorLog: radix squared).	"binary recursion like ipow"
	^x + (self / (radix raisedTo: x) floorLog: radix)

"""
    // 9 .. 38
    let expected = [112, 16, 178, 153, 117, 124, 112, 16, 208, 178, 153, 118, 124, 119, 112, 16, 208, 225, 184, 105, 17, 112, 16, 17, 226, 185, 16, 225, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testLog() throws {
    let source = """
log
	"Base 10 logarithm"

	^self ln / 10.0 ln

"""
    // 7 .. 12
    let expected = [112, 208, 33, 208, 185, 124]
    try runningSource(source, expecting: expected)
  }

  func testExponent() throws {
    let source = """
exponent
	"Consider the receiver to be represented as a power of two multiplied by a
	mantissa (between one and two).  Answer with the SmallInteger to whose power
	two is raised.  Optional.  See Object documentation whatIsAPrimitive."

	| positive |
	<primitive: 53>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue:
			[positive _ (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent

"""
    // 19 .. 62
    let expected = [112, 33, 181, 155, 112, 119, 224, 124, 112, 37, 179, 172, 21, 33, 112, 185, 210, 104, 112, 33, 33, 16, 228, 185, 182, 154, 16, 211, 124, 16, 211, 118, 177, 124, 112, 37, 182, 153, 116, 124, 112, 211, 210, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOnDigits() throws {
    let source = """
printOn: aStream digits: digits
	"Print me using digits significant figures."

	self > 0.0
		ifTrue: [self absPrintOn: aStream digits: digits]
		ifFalse: [self = 0.0
					ifTrue:
						[aStream nextPutAll: '0.0']
					ifFalse:
						[aStream nextPutAll: '-'.
						self negated absPrintOn: aStream digits: digits]]

"""
    // 15 .. 44
    let expected = [112, 37, 179, 157, 112, 16, 17, 242, 164, 18, 112, 37, 182, 156, 16, 36, 224, 164, 9, 16, 33, 224, 135, 112, 211, 16, 17, 242, 135, 120]
    try runningSource(source, expecting: expected)
  }

// TODO: Puts a "1" into the literals!
//   func testAbsPrintOnDigits() throws {
//     let source = """
// absPrintOn: aStream digits: digits
// 	"Print me using digits significant figures."
//
// 	| fuzz x exp q i |
// 	"x is myself normalized to [1.0, 10.0), exp is my exponent"
// 	exp _
// 		self < 1.0
// 			ifTrue: [(10.0 / self floorLog: 10.0) negated]
// 			ifFalse: [self floorLog: 10.0].
// 	x _ self / (10.0 raisedTo: exp).
// 	fuzz _ 10.0 raisedTo: 1 - digits.
// 	"round the last digit to be printed"
// 	x _ 0.5 * fuzz + x.
// 	x >= 10.0
// 		ifTrue:
// 			["check if rounding has unnormalized x"
// 			x _ x / 10.0.
// 			exp _ exp + 1].
// 	(exp < 6 and: [exp > -4])
// 		ifTrue:
// 			["decimal notation"
// 			q _ 0.
// 			exp < 0 ifTrue: [1 to: 1 - exp do: [:i | aStream nextPut: ('0.0000' at: i)]]]
// 		ifFalse:
// 			["scientific notation"
// 			q _ exp.
// 			exp _ 0].
// 	[x >= fuzz]
// 		whileTrue:
// 			["use fuzz to track significance"
// 			i _ x truncated.
// 			aStream nextPut: (48 + i) asCharacter.
// 			x _ x - i * 10.0.
// 			fuzz _ fuzz * 10.0.
// 			exp _ exp - 1.
// 			exp = -1 ifTrue: [aStream nextPut: $.]].
// 	[exp >= -1]
// 		whileTrue:
// 			[aStream nextPut: $0.
// 			exp _ exp - 1.
// 			exp = -1 ifTrue: [aStream nextPut: $.]].
// 	q ~= 0
// 		ifTrue:
// 			[aStream nextPut: $e.
// 			q printOn: aStream]
//
// """
//     // 39 .. 204
//     let expected = [112, 35, 178, 158, 33, 112, 185, 33, 224, 210, 146, 112, 33, 224, 108, 112, 33, 20, 228, 185, 107, 33, 118, 17, 177, 228, 106, 37, 18, 184, 19, 176, 107, 19, 33, 181, 159, 19, 33, 185, 107, 20, 118, 176, 108, 20, 42, 178, 155, 20, 41, 179, 144, 114, 172, 27, 117, 109, 20, 117, 178, 172, 18, 118, 118, 20, 177, 137, 40, 200, 164, 7, 110, 16, 39, 22, 192, 196, 125, 246, 144, 115, 148, 20, 109, 117, 129, 68, 135, 19, 18, 181, 172, 34, 19, 219, 110, 16, 45, 22, 176, 220, 196, 135, 19, 22, 177, 33, 184, 107, 18, 33, 184, 106, 20, 118, 177, 108, 20, 116, 182, 155, 16, 46, 196, 135, 163, 217, 20, 116, 181, 172, 18, 16, 47, 196, 135, 20, 40, 177, 108, 20, 116, 182, 155, 16, 46, 196, 135, 163, 233, 21, 117, 183, 172, 9, 16, 48, 196, 135, 21, 16, 131, 49, 135, 120]
//     try runningSource(source, expecting: expected)
//   }

  func testTimesTwoPower() throws {
    let source = """
timesTwoPower: anInteger
	"Answer with the receiver mulitplied by 2.0 raised to the power of the argument.
	Optional.  See Object documentation whatIsAPrimitive."

	<primitive: 54>
	^self * (2.0 raisedToInteger: anInteger)

"""
    // 11 .. 16
    let expected = [112, 33, 16, 224, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testSin() throws {
    let source = """
sin
	"Answers with the sine of the receiver in radians."

	| x x2 sum |
		"normalize to 0<=self<=(Pi/2)"
	self < 0.0 ifTrue: [^self negated sin negated].
	self > Twopi ifTrue: [^(self \\\\ Twopi) sin].
	self > Pi ifTrue: [^(self - Pi) sin negated].
	self > Halfpi ifTrue: [^(Pi - self) sin].
	sum _ x _ self.
	x2 _ x * x.
	SinCoefficients do: [:const | sum _ const * (x _ x * x2) + sum].
	^sum

"""
    // 17 .. 84
    let expected = [112, 34, 178, 156, 112, 208, 209, 208, 124, 112, 67, 179, 156, 112, 67, 186, 209, 124, 112, 68, 179, 157, 112, 68, 177, 209, 208, 124, 112, 69, 179, 156, 68, 112, 177, 209, 124, 112, 129, 64, 106, 16, 16, 184, 105, 70, 137, 118, 200, 164, 13, 107, 19, 16, 17, 184, 129, 64, 184, 18, 176, 129, 66, 125, 203, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }


  func testPrintOn() throws {
    let source = """
printOn: aStream
	self printOn: aStream digits: 6

"""
    // 7 .. 12
    let expected = [112, 16, 33, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNotEquals() throws {
    let source = """
~= aNumber
	"Compare the receiver with the argument and return true if the receiver is not
	equal to the argument.  Otherwise return false.  Fail if the argument is not a
	Float.  Optional.  See Object documentation whatIsAPrimitive."

	<primitive: 48>
	^super ~= aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testCos() throws {
    let source = """
cos
	"Answers with the cosine of the receiver in radians."

	self < 0.0 ifTrue: [^(self + Halfpi) sin].
	^(Halfpi - self) sin

"""
    // 9 .. 22
    let expected = [112, 34, 178, 156, 112, 65, 176, 208, 124, 65, 112, 177, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Hash is reimplemented because = is implemented."

	^(self basicAt: 1) bitAnd: 16383		"High bits as an Integer"

"""
    // 7 .. 12
    let expected = [112, 118, 224, 33, 190, 124]
    try runningSource(source, expecting: expected)
  }

  func testRadiansToDegrees() throws {
    let source = """
radiansToDegrees
	^self / RadiansPerDegree

"""
    // 5 .. 8
    let expected = [112, 64, 185, 124]
    try runningSource(source, expecting: expected)
  }

  func testArcSin() throws {
    let source = """
arcSin
	"Answers with the angle in radians"

	self abs > 1.0 ifTrue: [self error: 'Value out of range'].
	self abs = 1.0
		ifTrue: [^Halfpi]
		ifFalse: [^(self / (1 - (self * self)) sqrt) arcTan]

"""
    // 17 .. 42
    let expected = [112, 210, 35, 179, 155, 112, 33, 224, 135, 112, 210, 35, 182, 153, 70, 124, 112, 118, 112, 112, 184, 177, 213, 185, 212, 124]
    try runningSource(source, expecting: expected)
  }

  func testSqrt() throws {
    let source = """
sqrt
	| guess |
	self <= 0.0 ifTrue: [self = 0.0
			ifTrue: [^0.0]
			ifFalse: [^self error: 'sqrt invalid for x < 0']].
	"copy and halve the exponent for first guess"
	guess _ self timesTwoPower: 0 - (self exponent // 2).
	5 timesRepeat: [guess _ self - (guess * guess) / (guess * 2.0) + guess].
	^guess

"""
    // 19 .. 66
    let expected = [112, 34, 180, 172, 10, 112, 34, 182, 153, 34, 124, 112, 33, 224, 124, 112, 117, 112, 212, 119, 189, 177, 227, 104, 38, 137, 117, 200, 164, 14, 112, 16, 16, 184, 177, 16, 39, 184, 185, 16, 176, 129, 64, 125, 229, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testArcTan() throws {
    let source = """
arcTan
	"Answers with the angle in radians"

	| theta term y eps i |
	self = 1.0 ifTrue: [^Fourthpi].
	self = -1.0 ifTrue: [^Fourthpi negated].
	self * self > 1.0
		ifTrue:
			[theta _ Halfpi.
			y _ -1.0 / (self * self).
			term _ -1.0 / self abs]
		ifFalse:
			[theta _ 0.0.
			y _ 0.0 - (self * self).
			term _ self abs].
	i _ 1.
	eps _ 1.0e-4.
	[term abs > eps]
		whileTrue:
			[theta _ theta + term.
			term _ term * y * i asFloat / (i + 2) asFloat.
			i _ i + 2].
	^self sign asFloat * theta

"""
    // 23 .. 109
    let expected = [112, 33, 182, 153, 64, 124, 112, 35, 182, 154, 64, 210, 124, 112, 112, 184, 33, 179, 172, 16, 70, 104, 35, 112, 112, 184, 185, 106, 35, 112, 213, 185, 129, 65, 164, 12, 36, 104, 36, 112, 112, 184, 177, 106, 112, 213, 129, 65, 135, 118, 108, 39, 107, 17, 213, 19, 179, 172, 22, 16, 17, 176, 104, 17, 18, 184, 20, 216, 184, 20, 119, 176, 216, 185, 105, 20, 119, 176, 108, 163, 228, 112, 217, 216, 16, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testTruncated() throws {
    let source = """
truncated
	"Answer with a SmallInteger equal to the value of the receiver without
	its fractional part. The primitive fails if the truncated value cannot be
	represented as a SmallInteger. In that case, the code below will compute
	a LargeInteger truncated value. Essential. See Object documentation
	whatIsAPrimitive. "

	<primitive: 51>
	^(self quo: 16383.0) * 16383 + (self rem: 16383.0) truncated

"""
    // 17 .. 27
    let expected = [112, 33, 224, 34, 184, 112, 33, 228, 211, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= aNumber
	"Compare the receiver with the argument and return true if the receiver is
	equal to the argument.  Otherwise return false.  Fail if the argument is not a
	Float.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 47>
	^self retry: #= coercing: aNumber

"""
    // 11 .. 15
    let expected = [112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testExp() throws {
    let source = """
exp
	"see Computer Approximations, pp. 96-104, p. 205 (EXPB 1065)"

	| a n1 x x2 P Q |
	self abs > 9212.0
		ifTrue: ["Float maxVal ln"
			"1.0 exp"
			self error: 'exp overflow']
		ifFalse:
			[x _ self / Ln2.
			n1 _ 2.0 raisedTo: x truncated.
			(x _ x - x truncated) >= 0.5
				ifTrue:
					[n1 _ n1 * Sqrt2.
					x _ x - 0.5].
			x2 _ x * x.
			"compute 2.0 power: x"
			P _ Q _ 0.0.
			ExpPCoefficients do: [:a | P _ P * x2 + a].
			ExpQCoefficients do: [:a | Q _ Q * x2 + a].
			^n1 * (Q + (x * P) / (Q - (x * P)))]

"""
    // 29 .. 122
    let expected = [112, 219, 44, 179, 156, 112, 42, 233, 164, 82, 112, 64, 185, 106, 34, 18, 211, 225, 105, 18, 18, 211, 177, 129, 66, 37, 181, 159, 17, 68, 184, 105, 18, 37, 177, 106, 18, 18, 184, 107, 38, 129, 69, 108, 71, 137, 118, 200, 164, 9, 104, 20, 19, 184, 16, 176, 129, 68, 125, 203, 135, 72, 137, 118, 200, 164, 9, 104, 21, 19, 184, 16, 176, 129, 69, 125, 203, 135, 17, 21, 18, 20, 184, 176, 21, 18, 20, 184, 177, 185, 184, 124, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testArcCos() throws {
    let source = """
arcCos
	"Answers with the angle in radians."

	^Halfpi - self arcSin

"""
    // 7 .. 11
    let expected = [64, 112, 209, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testTan() throws {
    let source = """
tan
	"Answer the ratio of the sine to cosine of the receiver in radians."

	| x x2 sum |
		"normalize to 0<=self<=(Pi/4)"
	self < 0.0 ifTrue: [^self negated tan negated].
	self > Pi ifTrue: [^(self \\\\ Pi) tan].
	self > Halfpi ifTrue: [^(Pi - self) tan negated].
	self > Fourthpi ifTrue: [^1.0 / (Halfpi - self) tan].
	sum _ x _ self.
	x2 _ x * x.
	TanCoefficients do: [:const | sum _ const * (x _ x * x2) + sum].
	^sum

"""
    // 19 .. 88
    let expected = [112, 34, 178, 156, 112, 208, 209, 208, 124, 112, 67, 179, 156, 112, 67, 186, 209, 124, 112, 68, 179, 157, 67, 112, 177, 209, 208, 124, 112, 70, 179, 158, 37, 68, 112, 177, 209, 185, 124, 112, 129, 64, 106, 16, 16, 184, 105, 71, 137, 118, 200, 164, 13, 107, 19, 16, 17, 184, 129, 64, 184, 18, 176, 129, 66, 125, 203, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testDeepCopy() throws {
    let source = """
deepCopy
	^self copy

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testLn() throws {
    let source = """
ln
	"see Computer Approximations, pp. 105-111, p. 227 (LOGE 2663)"

	| expt x x2 n P |
	self <= 0.0
		ifTrue: [self error: 'ln not valid for ' , self printString]
		ifFalse:
			[expt _ self exponent.
			n _ Ln2 * (expt - 0.5).
			"mantissa between 0.5 and 1.0"
			x _ self timesTwoPower: 0 - expt.
			x _ x * Sqrt2.
			x _ x - 1.0 / (x + 1.0).
			x2 _ x * x.
			P _ 0.0.
			LnCoefficients do: [:a | P _ P * x2 + a].
			^n + (x * P)]

"2.718284 ln 1.0"

"""
    // 27 .. 96
    let expected = [112, 38, 180, 159, 112, 42, 112, 219, 233, 232, 164, 56, 112, 208, 104, 65, 16, 34, 177, 184, 107, 112, 117, 16, 177, 227, 105, 17, 68, 184, 105, 17, 37, 177, 17, 37, 176, 185, 105, 17, 17, 184, 106, 38, 108, 71, 137, 118, 200, 164, 9, 109, 20, 18, 184, 21, 176, 129, 68, 125, 203, 135, 19, 17, 20, 184, 176, 124, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMultiply() throws {
    let source = """
* aNumber
	"Multiply the receiver by the argument and return the result as a Float.  Fail if
	the argument is not a Float.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 49>
	^self retry: #* coercing: aNumber

"""
    // 11 .. 15
    let expected = [112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testNegated() throws {
    let source = """
negated
	"Answer a Number that is the negation of the receiver."
	^0.0 - self

"""
    // 5 .. 8
    let expected = [32, 112, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ aNumber
	"Divide the receiver by the argument and return the exact result as a Float.  Fail
	if the argument is not a Float.  Essential.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 50>
	aNumber = 0
		ifTrue: [self error: 'attempt to divide by zero']
		ifFalse: [^self retry: #/ coercing: aNumber]
"""
    // 15 .. 29
    let expected = [16, 117, 182, 155, 112, 35, 226, 148, 112, 33, 16, 240, 124, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPlus() throws {
    let source = """
+ aNumber
	"Add the receiver to the argument and return the result as a Float.  Fail if the
	argument is not a Float.  Essential.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 41>
	^self retry: #+ coercing: aNumber

"""
    // 11 .. 15
    let expected = [112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsFraction() throws {
    let source = """
asFraction
	"Answer with a new Fraction representing the receiver.
	This conversion uses the continued fraction method to approximate
	a floating point number."

	| num1 denom1 num2 denom2 int frac newD temp |
	num1 _ self truncated.	"The first of two alternating numerators"
	denom1 _ 1.		"The first of two alternating denominators"
	num2 _ 1.		"The second numerator"
	denom2 _ 0.		"The second denominator--will update"
	int _ num1.		"The integer part of self"
	frac _ self fractionPart.		"The fractional part of self"
	[frac = 0]
		whileFalse:
			["repeat while the fractional part is not zero"
			newD _ 1.0 / frac.			"Take reciprocal of the fractional part"
			int _ newD truncated.		"get the integer part of this"
			frac _ newD fractionPart.	"and save the fractional part for next time"
			temp _ num2.				"Get old numerator and save it"
			num2 _ num1.				"Set second numerator to first"
			num1 _ num1 * int + temp.	"Update first numerator"
			temp _ denom2.				"Get old denominator and save it"
			denom2 _ denom1.			"Set second denominator to first"
			denom1 _ int * denom1 + temp.		"Update first denominator"
			10000.0 < denom1
				ifTrue:
					["Is ratio past float precision?  If so, pick which
					of the two ratios to use"
					num2 = 0.0
						ifTrue: ["Is second denominator 0?"
								^Fraction numerator: num1 denominator: denom1].
					^Fraction numerator: num2 denominator: denom2]].
	"If fractional part is zero, return the first ratio"
	denom1 = 1
		ifTrue: ["Am i really an Integer?"
				^num1"Yes, return Integer result"]
		ifFalse: ["Otherwise return Fraction result"
				^Fraction numerator: num1 denominator: denom1]

"""
    // 17 .. 97
    let expected = [112, 208, 104, 118, 105, 118, 106, 117, 107, 16, 108, 112, 209, 109, 21, 117, 182, 168, 51, 34, 21, 185, 110, 22, 208, 108, 22, 209, 109, 18, 111, 16, 106, 16, 20, 184, 23, 176, 104, 19, 111, 17, 107, 20, 17, 184, 23, 176, 105, 38, 17, 178, 172, 14, 18, 37, 182, 156, 68, 16, 17, 243, 124, 68, 18, 19, 243, 124, 163, 200, 17, 118, 182, 153, 16, 124, 68, 16, 17, 243, 124]
    try runningSource(source, expecting: expected)
  }

  func testDegreesToRadians() throws {
    let source = """
degreesToRadians
	^self * RadiansPerDegree

"""
    // 5 .. 8
    let expected = [112, 64, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testGenerality() throws {
    let source = """
generality
	^80

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testShallowCopy() throws {
    let source = """
shallowCopy
	^self + 0.0

"""
    // 5 .. 8
    let expected = [112, 32, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testCoerce() throws {
    let source = """
coerce: aNumber
	^aNumber asFloat

"""
    // 5 .. 7
    let expected = [16, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- aNumber
	"Subtract the argument from the receiver and return the result as a Float.  Fail if
	the argument is not a Float.  Essential.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 42>
	^self retry: #- coercing: aNumber

"""
    // 11 .. 15
    let expected = [112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aNumber
	"Compare the receiver with the argument and return true if the receiver is less
	than the argument.  Otherwise return false.  Fail if the argument is not a Float.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 43>
	^self retry: #< coercing: aNumber

"""
    // 11 .. 15
    let expected = [112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= aNumber
	"Compare the receiver with the argument and return true if the receiver is less
	than or equal to the argument.  Otherwise return false.  Fail if the argument is
	not a Float.  Optional.  See Object documentation whatIsAPrimitive."

	<primitive: 45>
	^super <= aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= aNumber
	"Compare the receiver with the argument and return true if the receiver is
	greater than or equal to the argument.  Otherwise return false.  Fail if the
	argument is not a Float.  Optional.  See Object documentation
	whatIsAPrimitive. "

	<primitive: 46>
	^super >= aNumber

"""
    // 9 .. 13
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> aNumber
	"Compare the receiver with the argument and return true if the receiver is
	greater than the argument.  Otherwise return false.  Fail if the argument is not a
	Float.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 44>
	^self retry: #> coercing: aNumber

"""
    // 11 .. 15
    let expected = [112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testRounded() throws {
    let source = """
rounded
	"Answer the integer nearest the receiver."
	self >= 0.0
		ifTrue: [^(self + 0.5) truncated]
		ifFalse: [^(self - 0.5) truncated]

"""
    // 9 .. 22
    let expected = [112, 34, 181, 156, 112, 33, 176, 208, 124, 112, 33, 177, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsLiteral() throws {
    let source = """
isLiteral
	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

  func testIntegerPart() throws {
    let source = """
integerPart
	"Answer with a new Float whose value is the receiver's truncated value."

	^self - self fractionPart

"""
    // 5 .. 9
    let expected = [112, 112, 208, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testFractionPart() throws {
    let source = """
fractionPart
	"Answer a new Float whose value is the difference between the receiver and the
	receiver's truncated value.  Optional.  See Object documentation
	whatIsAPrimitive."

	<primitive: 52>
	^self - self truncated

"""
    // 9 .. 13
    let expected = [112, 112, 208, 177, 124]
    try runningSource(source, expecting: expected)
  }

}
