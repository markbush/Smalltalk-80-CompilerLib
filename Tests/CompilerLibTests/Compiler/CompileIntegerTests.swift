import XCTest
@testable import CompilerLib

final class CompileIntegerTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Integer", instanceVariables: [])
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

  func testCoerce() throws {
    let source = """
coerce: aNumber
	^aNumber truncated

"""
    // 5 .. 7
    let expected = [16, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testEven() throws {
    let source = """
even
	^((self digitAt: 1) bitAnd: 1) = 0

"""
    // 5 .. 12
    let expected = [112, 118, 224, 118, 190, 117, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- aNumber
	"Subtract the argument from the receiver and answer with the result."

	aNumber isInteger
		ifTrue: [self negative == aNumber negative
					ifTrue: [^self digitSubtract: aNumber]
					ifFalse: [^self digitAdd: aNumber]]
		ifFalse: [^self retry: #- coercing: aNumber]

"""
    // 15 .. 37
    let expected = [16, 213, 172, 14, 112, 212, 16, 212, 198, 155, 112, 16, 227, 124, 112, 16, 226, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }


  func testBitXor() throws {
    let source = """
bitXor: aNumber
	"Logical XOR the twos-complement representation of the receiver with the
	twos-complement representation of the argument and return the result."

	| anInteger |
	anInteger _ aNumber truncated.
	^self
		digitLogic: anInteger
		op: #bitXor:
		length: (self digitLength max: anInteger digitLength)

"""
    // 13 .. 26
    let expected = [16, 208, 105, 112, 17, 34, 112, 212, 17, 212, 227, 131, 97, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsFloat() throws {
    let source = """
asFloat
	"Answer with a Float that represents the receiver."

	| factor sum |
	sum _ 0.0.
	factor _ self sign asFloat.
	1 to: self size do:
		[:i |
		sum _ (self digitAt: i) * factor + sum.
		factor _ factor * 256.0].
	^sum

"""
    // 15 .. 47
    let expected = [32, 105, 112, 210, 209, 104, 118, 112, 194, 137, 118, 200, 164, 15, 106, 112, 18, 228, 16, 184, 17, 176, 105, 16, 37, 184, 129, 64, 125, 243, 135, 17, 124]
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

  func testHighBit() throws {
    let source = """
highBit
	"Answer with the index of the high order bit of the binary
	representation of the receiver."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testGrowby() throws {
    let source = """
growby: n
	^self growto: self digitLength + n

"""
    // 7 .. 13
    let expected = [112, 112, 209, 16, 176, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAnyBitTo() throws {
    let source = """
anyBitTo: pos
	"Answer true if any bit from 1 to pos is non-zero, for testing for loss of significant
	bits when shifting right"

	1 to: pos - 1 // 8 do:
		[:i | (self digitAt: i) ~= 0 ifTrue: [^true]].
	^(self digitAt: pos + 7 // 8) anyMask: (#(1 3 7 15 31 63 127 255) at: pos - 1 \\\\ 8 + 1)

"""
    // 15 .. 55
    let expected = [118, 16, 118, 177, 33, 189, 137, 118, 200, 164, 10, 105, 112, 17, 226, 117, 183, 152, 121, 115, 125, 240, 135, 112, 16, 36, 176, 33, 189, 226, 37, 16, 118, 177, 33, 186, 118, 176, 192, 227, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyto() throws {
    let source = """
copyto: x
	1 to: (self digitLength min: x digitLength)
		do: [:i | x digitAt: i put: (self digitAt: i)].
	^x

"""
    // 13 .. 35
    let expected = [118, 112, 210, 16, 210, 225, 137, 118, 200, 164, 8, 105, 16, 17, 112, 17, 228, 243, 125, 240, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAnyMask() throws {
    let source = """
anyMask: mask
	"Treat the argument as a bit mask.  Answer true if any of the
	bits that are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask)

"""
    // 3 .. 8
    let expected = [117, 112, 16, 190, 183, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsCharacter() throws {
    let source = """
asCharacter
	"Answer the Character whose value is the receiver."

	^Character value: self

"""
    // 5 .. 8
    let expected = [64, 112, 202, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitLogicOpLength() throws {
    let source = """
digitLogic: arg op: op length: len
	| result i neg1 neg2 rneg z1 z2 rz b1 b2 b rdigits |
	neg1 _ self negative.
	neg2 _ arg negative.
	rneg _
		((neg1 ifTrue: [-1] ifFalse: [0])
			perform: op
			with: (neg2
					ifTrue: [-1]
					ifFalse: [0])) < 0.
	result _ Integer new: len neg: rneg.
	rz _ z1 _ z2 _ true.
	rdigits _ 1.
	1 to: result digitLength do:
		[:i |
		b1 _ self digitAt: i.
		neg1
			ifTrue: [b1 _ z1
						ifTrue: [b1 = 0
									ifTrue: [0]
									ifFalse:
										[z1 _ false.
										256 - b1]]
						ifFalse: [255 - b1]].
		b2 _ arg digitAt: i.
		neg2
			ifTrue: [b2 _ z2
						ifTrue: [b2 = 0
									ifTrue: [0]
									ifFalse:
										[z2 _ false.
										256 - b2]]
						ifFalse: [255 - b2]].
		b _ b1 perform: op with: b2.
		b = 0
			ifTrue:
				[result digitAt: i put: 0]
			ifFalse:
				[rdigits _ i.
				result
					digitAt: i
					put: (rneg
							ifTrue: [rz ifTrue:
											[rz _ false.
											256 - b]
										ifFalse: [255 - b]]
						ifFalse: [b])]].
	rdigits ~= result digitLength ifTrue: [^(result growto: rdigits) truncated].
	^result truncated

"""
    // 27 .. 184
    let expected = [112, 208, 109, 16, 208, 110, 21, 153, 116, 144, 117, 17, 22, 153, 116, 144, 117, 241, 117, 178, 111, 67, 18, 23, 242, 107, 113, 129, 73, 129, 72, 130, 74, 118, 130, 78, 118, 19, 213, 137, 118, 200, 164, 99, 108, 112, 20, 230, 130, 75, 21, 172, 21, 24, 172, 13, 27, 117, 182, 153, 117, 149, 114, 130, 72, 40, 27, 177, 146, 39, 27, 177, 130, 75, 16, 20, 230, 130, 76, 22, 172, 21, 25, 172, 13, 28, 117, 182, 153, 117, 149, 114, 130, 73, 40, 28, 177, 146, 39, 28, 177, 130, 76, 27, 17, 28, 241, 130, 77, 29, 117, 182, 157, 19, 20, 117, 249, 164, 23, 20, 130, 78, 19, 20, 23, 172, 13, 26, 158, 114, 130, 74, 40, 29, 177, 146, 39, 29, 177, 144, 29, 249, 125, 244, 135, 30, 19, 213, 183, 156, 19, 30, 235, 218, 124, 19, 218, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitLshiftBytesLookfirst() throws {
    let source = """
digitLshift: n bytes: b lookfirst: a
	| x f m len r digit |
	"shift by 8*b+n bits, 0<=n<8.  a true means check for a leading zero byte in the
	result "
	x _ 0.
	f _ n - 8.
	m _ 255 bitShift: 0 - n.
	len _ self digitLength + 1 + b.
	(a and: [(self lastDigit bitShift: f) = 0])
		ifTrue: [len _ len - 1].
	r _ Integer new: len neg: self negative.
	1 to: b do: [:i | r digitAt: i put: 0].
	1 to: len - b do:
		[:i |
		digit _ self digitAt: i.
		r
			digitAt: i + b
			put: (((digit bitAnd: m) bitShift: n) bitOr: x).
		"Avoid values > 8 bits"
		x _ digit bitShift: f].
	^r

"""
    // 23 .. 117
    let expected = [117, 107, 16, 32, 177, 108, 33, 117, 16, 177, 188, 109, 112, 210, 118, 176, 17, 176, 110, 18, 158, 112, 211, 20, 188, 117, 182, 144, 114, 155, 22, 118, 177, 110, 69, 22, 112, 214, 244, 111, 118, 17, 137, 118, 200, 164, 7, 130, 73, 23, 25, 117, 248, 125, 247, 135, 118, 22, 17, 177, 137, 118, 200, 164, 26, 130, 73, 112, 25, 233, 130, 72, 23, 25, 17, 176, 24, 21, 190, 16, 188, 19, 191, 248, 135, 24, 20, 188, 129, 67, 125, 247, 135, 23, 124]
    try runningSource(source, expecting: expected)
  }

  func testLastDigit() throws {
    let source = """
lastDigit
	"Answer the last digit of the integer."

	^self digitAt: self digitLength

"""
    // 7 .. 11
    let expected = [112, 112, 209, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAllMask() throws {
    let source = """
allMask: mask
	"Treat the argument as a bit mask.  Answer true if all of the
	bits that are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask)

"""
    // 3 .. 8
    let expected = [16, 112, 16, 190, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testNoMask() throws {
    let source = """
noMask: mask
	"Treat the argument as a bit mask.  Answer true if none of the bits
	that are 1 in the argument are 1 in the receiver."

	^0 = (self bitAnd: mask)

"""
    // 3 .. 8
    let expected = [117, 112, 16, 190, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreStringRadix() throws {
    let source = """
storeStringRadix: radix
	"Answer a String representing the receiver as a base radix integer in Smalltalk syntax (e.g. 8r377)."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	radix ~= 10 ifTrue:
		[radix printOn: aStream.
		aStream nextPutAll: 'r'].
	self printOn: aStream base: radix.
	^aStream contents

"""
    // 23 .. 48
    let expected = [65, 66, 35, 205, 224, 105, 16, 39, 183, 159, 16, 17, 228, 135, 17, 38, 229, 135, 112, 17, 16, 248, 135, 17, 217, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintStringRadix() throws {
    let source = """
printStringRadix: radix
	"Answer a String representing the receiver as a base radix integer."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream base: radix.
	^aStream contents

"""
    // 15 .. 28
    let expected = [65, 66, 35, 205, 224, 105, 112, 17, 16, 244, 135, 17, 213, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitAnd() throws {
    let source = """
bitAnd: aNumber
	"Logical AND the twos-complement representation of the receiver with the
	twos-complement representation of the argument and return the result."

	| anInteger |
	anInteger _ aNumber truncated.
	^self
		digitLogic: anInteger
		op: #bitAnd:
		length: (self digitLength max: anInteger digitLength)

"""
    // 13 .. 26
    let expected = [16, 208, 105, 112, 17, 34, 112, 212, 17, 212, 227, 131, 97, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitOr() throws {
    let source = """
bitOr: aNumber
	"Logical OR the twos-complement representation of the receiver with the
	twos-complement representation of the argument and return the result."

	| anInteger |
	anInteger _ aNumber truncated.
	^self
		digitLogic: anInteger
		op: #bitOr:
		length: (self digitLength max: anInteger digitLength)

"""
    // 13 .. 26
    let expected = [16, 208, 105, 112, 17, 34, 112, 212, 17, 212, 227, 131, 97, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitShift() throws {
    let source = """
bitShift: anInteger
	"Logical SHIFT the twos-complement representation of the receiver with the
	twos-complement representation of the argument and return the result.
	Shift left if the argument is positive, right if the argument is negative.
	Zeros are shifted in from the right in left shifts.
	The sign bit is extended in right shifts."

	| result abs |
	anInteger >= 0 ifTrue: [^(self
			digitLshift: (anInteger bitAnd: 7)
			bytes: (anInteger bitShift: -3)
			lookfirst: true) truncated].
	abs _ 0 - anInteger.
	result _ (self
				digitRshift: (abs bitAnd: 7)
				bytes: (abs bitShift: -3)
				lookfirst: self digitLength) truncated.
	(self negative and: [self anyBitTo: abs])
		ifTrue: [result _ result - 1].
	^result

"""
    // 19 .. 67
    let expected = [16, 117, 181, 172, 12, 112, 16, 34, 190, 16, 35, 188, 113, 131, 97, 208, 124, 117, 16, 177, 106, 112, 18, 34, 190, 18, 35, 188, 112, 213, 131, 100, 208, 105, 112, 215, 155, 112, 18, 230, 144, 114, 155, 17, 118, 177, 105, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitRshiftBytesLookfirst() throws {
    let source = """
digitRshift: anInteger bytes: b lookfirst: a
	 "shift right 8*b+anInteger bits, 0<=n<8.  Discard all digits beyond a,
	and all zeroes at or below a."

	| n x i r f m digit count|
	n _ 0 - anInteger.
	x _ 0.
	f _ n + 8.
	i _ a.
	m _ 255 bitShift: 0 - f.
	digit _ self digitAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x _ digit bitShift: f "Can't exceed 8 bits".
		i _ i - 1.
		digit _ self digitAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r _ Integer new: i - b neg: self negative.
	count _ i.
	x _ (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:i | digit _ self digitAt: i + 1.
		r digitAt: i - b put: (((digit bitAnd: m) bitShift: f) bitOr: x)
			"Avoid values > 8 bits".
		x _ digit bitShift: n].
	^r

"""
    // 19 .. 141
    let expected = [117, 16, 177, 107, 117, 108, 19, 32, 176, 111, 18, 109, 33, 117, 23, 177, 188, 130, 72, 112, 21, 226, 130, 73, 25, 19, 188, 20, 191, 117, 182, 155, 21, 118, 183, 144, 114, 172, 15, 25, 23, 188, 108, 21, 118, 177, 109, 112, 21, 226, 130, 73, 163, 226, 21, 17, 180, 157, 68, 117, 112, 213, 243, 124, 68, 21, 17, 177, 112, 213, 243, 110, 21, 130, 74, 112, 17, 118, 176, 226, 19, 188, 108, 17, 118, 176, 26, 137, 118, 200, 164, 27, 109, 112, 21, 118, 176, 226, 130, 73, 22, 21, 17, 177, 25, 24, 190, 23, 188, 20, 191, 247, 135, 25, 19, 188, 129, 68, 125, 246, 135, 22, 124]
    try runningSource(source, expecting: expected)
  }

  func testLastDigitGet() throws {
    let source = """
lastDigitGet: digit
	"Store the argument, digit, as the last digit of the integer."

	^self at: self digitLength put: digit

"""
    // 5 .. 10
    let expected = [112, 112, 208, 16, 193, 124]
    try runningSource(source, expecting: expected)
  }

  func testGrowto() throws {
    let source = """
growto: n
	^self copyto: (self species new: n)

"""
    // 7 .. 13
    let expected = [112, 112, 209, 16, 205, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testBitInvert() throws {
    let source = """
bitInvert
	"Answer an integer whose bits are the complement of the receiver."

	^-1 - self

"""
    // 3 .. 6
    let expected = [116, 112, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitCompare() throws {
    let source = """
digitCompare: arg
	| len arglen t5 t6 |
	len _ self digitLength.
	(arglen _ arg digitLength) ~= len
		ifTrue: [arglen > len
					ifTrue: [^-1]
					ifFalse: [^1]].
	[len > 0]
		whileTrue:
			[(t5 _ arg digitAt: len) ~= (t6 _ self digitAt: len)
				ifTrue: [t5 < t6
							ifTrue: [^1]
							ifFalse: [^-1]].
			len _ len - 1].
	^0

"""
    // 7 .. 57
    let expected = [112, 208, 105, 16, 208, 129, 66, 17, 183, 159, 18, 17, 179, 153, 116, 124, 118, 124, 17, 117, 179, 172, 26, 16, 17, 225, 129, 67, 112, 17, 225, 129, 68, 183, 159, 19, 20, 178, 153, 118, 124, 116, 124, 17, 118, 177, 105, 163, 225, 117, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitAdd() throws {
    let source = """
digitAdd: arg
	| len arglen i accum sum |
	accum _ 0.
	(len _ self digitLength) < (arglen _ arg digitLength) ifTrue: [len _ arglen].
	"Open code max: for speed"
	sum _ Integer new: len neg: self negative.
	i _ 1.
	[i <= len]
		whileTrue:
			[accum _ (accum bitShift: -8) + (self digitAt: i) + (arg digitAt: i).
			sum digitAt: i put: (accum bitAnd: 255).
			i _ i + 1].
	accum > 255
		ifTrue:
			[sum _ sum growby: 1.
			sum lastDigitGet: (accum bitShift: -8)].
	^sum

"""
    // 23 .. 91
    let expected = [117, 108, 112, 208, 129, 65, 16, 208, 129, 66, 178, 153, 18, 105, 66, 17, 112, 211, 241, 109, 118, 107, 19, 17, 180, 172, 25, 20, 36, 188, 112, 19, 229, 176, 16, 19, 229, 176, 108, 21, 19, 20, 39, 190, 246, 135, 19, 118, 176, 107, 163, 226, 20, 39, 179, 172, 10, 21, 118, 232, 109, 21, 20, 36, 188, 233, 135, 21, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsFraction() throws {
    let source = """
asFraction
	"Answer with a Fraction that represents the receiver."

	^Fraction numerator: self denominator: 1

"""
    // 7 .. 11
    let expected = [65, 112, 118, 240, 124]
    try runningSource(source, expecting: expected)
  }


  func testDenominator() throws {
    let source = """
denominator
	^1

"""
    // 3 .. 4
    let expected = [118, 124]
    try runningSource(source, expecting: expected)
  }


  func testQuo() throws {
    let source = """
quo: aNumber
	"Divide the receiver by the argument and return the result.
	Round the result down towards zero to make it a whole integer."

	| ng quo |
	aNumber isInteger
		ifTrue:
			[ng _ self negative == aNumber negative == false.
			quo _ (self digitDiv: aNumber neg: ng) at: 1.
			(quo lastDigit = 0 and: [quo digitLength >= 2])
				ifTrue: [^(quo growby: -1) truncated].
			^quo truncated]
		ifFalse: [^self retry: #quo: coercing: aNumber]

"""
    // 21 .. 64
    let expected = [16, 216, 172, 35, 112, 210, 16, 210, 198, 114, 198, 105, 112, 16, 17, 243, 118, 192, 106, 18, 215, 117, 182, 156, 18, 214, 119, 181, 144, 114, 156, 18, 116, 229, 212, 124, 18, 212, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitSubtract() throws {
    let source = """
digitSubtract: arg
	| smaller larger i z sum sl al ng lastdigit |
	sl _ self digitLength.
	al _ arg digitLength.
	(sl = al
		ifTrue:
			[[(self digitAt: sl) = (arg digitAt: sl) and: [sl > 1]]
				whileTrue: [sl _ sl - 1].
			al _ sl.
			(self digitAt: sl) < (arg digitAt: sl)]
		ifFalse: [sl < al])
		ifTrue:
			[larger _ arg.
			smaller _ self.
			ng _ self negative == false.
			sl _ al]
		ifFalse:
			[larger _ self.
			smaller _ arg.
			ng _ self negative].
	sum _ Integer new: sl neg: ng.
	lastdigit _ 1.
	z _ 0.
	"Loop invariant is -1<=z<=1"
	i _ 1.
	[i <= sl]
		whileTrue:
			[z _ z + (larger digitAt: i) - (smaller digitAt: i).
			(sum digitAt: i put: (z bitAnd: 255)) ~= 0 ifTrue: [lastdigit _ i].
			z _ z bitShift: -8.
			i _ i + 1].
	lastdigit = sl ifFalse: [sum _ sum growto: lastdigit].
	^sum truncated

"""
    // 23 .. 152
    let expected = [112, 208, 110, 16, 208, 111, 22, 23, 182, 172, 30, 112, 22, 226, 16, 22, 226, 182, 155, 22, 118, 179, 144, 114, 157, 22, 118, 177, 110, 163, 236, 22, 111, 112, 22, 226, 16, 22, 226, 178, 146, 22, 23, 178, 172, 14, 16, 106, 112, 105, 112, 209, 114, 198, 130, 72, 23, 129, 70, 151, 112, 106, 16, 105, 112, 209, 129, 72, 135, 68, 22, 24, 243, 109, 118, 130, 73, 117, 108, 118, 107, 19, 22, 180, 172, 32, 20, 18, 19, 226, 176, 17, 19, 226, 177, 108, 21, 19, 20, 38, 190, 245, 117, 183, 154, 19, 130, 73, 20, 39, 188, 108, 19, 118, 176, 107, 163, 219, 25, 22, 182, 168, 4, 21, 25, 232, 109, 21, 217, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitMultiplyNeg() throws {
    let source = """
digitMultiply: arg neg: ng
	| prod pl carry digit i j k xh xl low high |
	((arg digitAt: 1) = 0 and: [arg digitLength = 1]) ifTrue: [^0].
	pl _ self digitLength + arg digitLength.
	prod _ Integer new: pl neg: ng.
	"prod starts out all zero"
	1 to: self digitLength do:
		[:i |
		(digit _ self digitAt: i) ~= 0
			ifTrue:
				[k _ i.
				carry _ 0.
				xh _ digit bitShift: -4.
				xl _ digit bitAnd: 15.
				"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg digitLength do:
					[:j |
					high _ (arg digitAt: j) * xh.
					"Do double-precision multiply in two parts.
					Integers must be at least 13 bits for this to work."
					low _ (arg digitAt: j)
								* xl + ((high bitAnd: 15)
									bitShift: 4) + carry + (prod digitAt: k).
					carry _ (high bitShift: -4) + (low bitShift: -8).
					prod digitAt: k put: (low bitAnd: 255).
					k _ k + 1].
				prod digitAt: k put: carry]].
	(prod digitAt: pl) = 0 ifTrue: [^(prod growby: -1) truncated].
	^prod truncated

"""
    // 29 .. 168
    let expected = [16, 118, 225, 117, 182, 156, 16, 208, 118, 182, 144, 114, 153, 117, 124, 112, 208, 16, 208, 176, 107, 67, 19, 17, 242, 106, 118, 112, 208, 137, 118, 200, 164, 90, 110, 112, 22, 225, 129, 69, 117, 183, 172, 78, 22, 130, 72, 117, 108, 21, 37, 188, 130, 73, 21, 38, 190, 130, 74, 118, 16, 208, 137, 118, 200, 164, 48, 111, 16, 23, 225, 25, 184, 130, 76, 16, 23, 225, 26, 184, 28, 38, 190, 39, 188, 176, 20, 176, 18, 24, 225, 176, 130, 75, 28, 37, 188, 27, 40, 188, 176, 108, 18, 24, 27, 42, 190, 249, 135, 24, 118, 176, 129, 72, 125, 244, 135, 18, 24, 20, 249, 144, 115, 125, 244, 135, 18, 19, 225, 117, 182, 156, 18, 116, 236, 219, 124, 18, 219, 124]
    try runningSource(source, expecting: expected)
  }

  func testDigitDivNeg() throws {
    let source = """
digitDiv: arg neg: ng
	"Answer with an array of (quotient, remainder)"

	| quo rem ql d div dh dnh dl qhi qlo i j k l hi lo r3 a t |
	l _ self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^Array with: 0 with: self].
	d _ 8 - arg lastDigit highBit.
	rem _
		self	digitLshift: d
			bytes: 0
			lookfirst: false.
	"makes a copy and shifts"
	div _
		arg digitLshift: d
			bytes: 0
			lookfirst: false.
	"shifts so high order word is >=128"
	quo _ Integer new: l neg: ng.
	dl _ div digitLength - 1.
	"Last actual byte of data"
	ql _ l.
	dh _ div digitAt: dl.
	dnh _
		 dl = 1
			ifTrue: [0]
			ifFalse: [div digitAt: dl - 1].
	1 to: ql do:
		[:k |
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j _ rem digitLength + 1 - k.
		"r1 _ rem digitAt: j."
		(rem digitAt: j) = dh
			ifTrue: [qhi _ qlo _ 15"i.e. q=255"]
			ifFalse:
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.
				Note that r1,r2 are bytes, not nibbles.
				Be careful not to generate intermediate results exceeding 13 bits."
				"r2 _ (rem digitAt: j - 1)."
				t _ ((rem digitAt: j) bitShift: 4) + ((rem digitAt: j - 1) bitShift: -4).
				qhi _ t // dh.
				t _ (t \\\\ dh bitShift: 4) + ((rem digitAt: j - 1) bitAnd: 15).
				qlo _ t // dh.
				t _ t \\\\ dh.
				"Next compute (hi,lo) _ q*dnh"
				hi _ qhi * dnh.
				lo _ qlo * dnh + ((hi bitAnd: 15) bitShift: 4).
				hi _ (hi bitShift: -4) + (lo bitShift: -8).
				lo _ lo bitAnd: 255.
				"Correct overestimate of q.
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 _
					j < 3 ifTrue: [0]
						 ifFalse: [rem digitAt: j - 2].
				[(t < hi or: [t = hi and: [r3 < lo]]) and:
						["i.e. (t,r3) < (hi,lo)"
						qlo _ qlo - 1.
						lo _ lo - dnh.
						lo < 0
							ifTrue:
								[hi _ hi - 1.
								lo _ lo + 256].
						hi >= dh]]
					whileTrue: [hi _ hi - dh].
				qlo < 0
					ifTrue:
						[qhi _ qhi - 1.
						qlo _ qlo + 16]].
		"Subtract q*div from rem"
		l _ j - dl.
		a _ 0.
		1 to: div digitLength do:
			[:i |
			hi _ (div digitAt: i) * qhi.
			lo _
				a + (rem digitAt: l)
					- ((hi bitAnd: 15) bitShift: 4)
					- ((div digitAt: i) * qlo).
			rem digitAt: l put: (lo bitAnd: 255).
			a _ (lo bitShift: -8) - (hi bitShift: -4).
			l _ l + 1].
		a < 0
			ifTrue:
				["Add div back into rem, decrease q by 1"
				qlo _ qlo - 1.
				l _ j - dl.
				a _ 0.
				1 to: div digitLength do:
					[:i |
					a _ (a bitShift: -8) + (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l _ l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4) + qlo].
	rem _
		rem digitRshift: d
			bytes: 0
			lookfirst: dl.
	^Array with: quo with: rem

"""
    // 45 .. 487
    let expected = [112, 208, 16, 208, 177, 118, 176, 130, 79, 31, 117, 180, 156, 66, 117, 112, 241, 124, 35, 16, 213, 212, 177, 109, 112, 21, 117, 114, 131, 102, 107, 16, 21, 117, 114, 131, 102, 110, 72, 31, 17, 247, 106, 22, 208, 118, 177, 130, 73, 31, 108, 22, 25, 233, 111, 25, 118, 182, 153, 117, 148, 22, 25, 118, 177, 233, 130, 72, 118, 20, 137, 118, 200, 165, 98, 130, 78, 19, 208, 118, 176, 30, 177, 130, 77, 19, 29, 233, 23, 182, 158, 45, 129, 75, 129, 74, 164, 181, 19, 29, 233, 43, 188, 19, 29, 118, 177, 233, 44, 188, 176, 130, 84, 128, 84, 23, 189, 130, 74, 128, 84, 23, 186, 43, 188, 19, 29, 118, 177, 233, 45, 190, 176, 130, 84, 128, 84, 23, 189, 130, 75, 128, 84, 23, 186, 130, 84, 26, 24, 184, 130, 80, 27, 24, 184, 128, 80, 45, 190, 43, 188, 176, 130, 81, 128, 80, 44, 188, 128, 81, 46, 188, 176, 130, 80, 128, 81, 47, 190, 130, 81, 29, 48, 178, 153, 117, 148, 19, 29, 119, 177, 233, 130, 82, 128, 84, 128, 80, 178, 154, 113, 164, 13, 128, 84, 128, 80, 182, 157, 128, 82, 128, 81, 178, 144, 114, 172, 34, 27, 118, 177, 130, 75, 128, 81, 24, 177, 130, 81, 128, 81, 117, 178, 172, 12, 128, 80, 118, 177, 130, 80, 128, 81, 49, 176, 130, 81, 128, 80, 23, 181, 144, 114, 159, 128, 80, 23, 177, 130, 80, 163, 188, 27, 117, 178, 172, 11, 26, 118, 177, 130, 74, 27, 50, 176, 129, 75, 144, 115, 135, 29, 25, 177, 130, 79, 117, 130, 83, 118, 22, 208, 137, 118, 200, 164, 56, 130, 76, 22, 28, 233, 26, 184, 130, 80, 128, 83, 19, 31, 233, 176, 128, 80, 45, 190, 43, 188, 177, 22, 28, 233, 27, 184, 177, 130, 81, 19, 31, 128, 81, 47, 190, 131, 83, 135, 128, 81, 46, 188, 128, 80, 44, 188, 177, 130, 83, 31, 118, 176, 129, 79, 125, 250, 135, 128, 83, 117, 178, 172, 54, 27, 118, 177, 130, 75, 29, 25, 177, 130, 79, 117, 130, 83, 118, 22, 208, 137, 118, 200, 164, 31, 130, 76, 128, 83, 46, 188, 19, 31, 233, 176, 22, 28, 233, 176, 130, 83, 19, 31, 128, 83, 47, 190, 131, 83, 135, 31, 118, 176, 129, 79, 125, 250, 135, 18, 18, 208, 118, 176, 30, 177, 26, 43, 188, 27, 176, 131, 83, 125, 250, 135, 19, 21, 117, 25, 131, 116, 107, 66, 18, 19, 241, 124]
    try runningSource(source, expecting: expected)
  }

  func testLcm() throws {
    let source = """
lcm: n
	"Answer the least common multiple of the receiver and n."

	^self // (self gcd: n) * n

"""
    // 5 .. 12
    let expected = [112, 112, 16, 224, 189, 16, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testFactorial() throws {
    let source = """
factorial
	"Answer the factorial of the receiver.  For example, 6 factorial == 6*5*4*3*2*1.
	Signal an error if the receiver is less than 0."

	self > 0
		ifTrue: [^self * (self - 1) factorial].
	self = 0
		ifTrue: [^1].
	self error: 'factorial invalid for: ' , self printString

"""
    // 13 .. 37
    let expected = [112, 117, 179, 158, 112, 112, 118, 177, 208, 184, 124, 112, 117, 182, 153, 118, 124, 112, 35, 112, 212, 226, 225, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testBitAt() throws {
    let source = """
bitAt: i
	"Answer the bit at the ith position."

	^(self bitAnd: (1 bitShift: i - 1)) = 0
		ifTrue: [0]
		ifFalse: [1]

"""
    // 3 .. 16
    let expected = [112, 118, 16, 118, 177, 188, 190, 117, 182, 153, 117, 144, 118, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	self printOn: aStream base: 10	"default print radix"

"""
    // 7 .. 12
    let expected = [112, 16, 33, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNotEquals() throws {
    let source = """
~= anInteger
	"Compare the receiver with the argument and return true if the receiver is
	not equal to the argument. Otherwise answer false."

	^super ~= anInteger

"""
    // 7 .. 11
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testTimesRepeat() throws {
    let source = """
timesRepeat: aBlock
	"Evaluate the argument, aBlock, the number of times represented by
	the receiver."

	| count |
	count _ 1.
	[count <= self]
		whileTrue:
			[aBlock value.
			count _ count + 1]

"""
    // 3 .. 19
    let expected = [118, 105, 17, 112, 180, 172, 9, 16, 201, 135, 17, 118, 176, 105, 163, 242, 120]
    try runningSource(source, expecting: expected)
  }

  func testGcd() throws {
    let source = """
gcd: anInteger
	"Answer the greatest common divisor of the receiver and anInteger.
	Uses Roland Silver's algorithm"

	| m n d t |
	m _ self abs max: anInteger abs.
	n _ self abs min: anInteger abs.
	m \\\\ n = 0 ifTrue: [^n].
	"easy test, speeds up rest"
	d _ 0.
	[n even and: [m even]]
		whileTrue:
			[d _ d + 1.
			n _ n bitShift: -1.
			m _ m bitShift: -1].
	[n even]
		whileTrue: [n _ n bitShift: -1].
	[m even]
		whileTrue: [m _ m bitShift: -1].
	[m = n]
		whileFalse:
			[m > n
				ifTrue:
					[m _ m - n]
				ifFalse:
					[t _ m.
					m _ n - m.
					n _ t].
			"Make sure larger gets replaced"
			[m even]
				whileTrue: [m _ m bitShift: -1]].
	d = 0 ifTrue: [^m].
	^m bitShift: d

"""
    // 11 .. 120
    let expected = [112, 209, 16, 209, 224, 105, 112, 209, 16, 209, 226, 106, 17, 18, 186, 117, 182, 153, 18, 124, 117, 107, 18, 211, 154, 17, 211, 144, 114, 172, 14, 19, 118, 176, 107, 18, 116, 188, 106, 17, 116, 188, 105, 163, 233, 18, 211, 157, 18, 116, 188, 106, 163, 247, 17, 211, 157, 17, 116, 188, 105, 163, 247, 17, 18, 182, 168, 32, 17, 18, 179, 158, 17, 18, 177, 129, 65, 164, 9, 17, 108, 18, 17, 177, 105, 20, 129, 66, 135, 17, 211, 157, 17, 116, 188, 105, 163, 247, 163, 219, 19, 117, 182, 153, 17, 124, 17, 19, 188, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	^(self lastDigit bitShift: 8) + (self digitAt: 1)

"""
    // 9 .. 17
    let expected = [112, 208, 33, 188, 112, 118, 226, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testMultiply() throws {
    let source = """
* aNumber
	"Multiply the receiver by the argument and answer with the result."

	aNumber isInteger
		ifTrue: [^(self
					digitMultiply: aNumber
					neg: self negative ~~ aNumber negative) truncated]
		ifFalse: [^self retry: #* coercing: aNumber]

"""
    // 17 .. 35
    let expected = [16, 214, 172, 10, 112, 16, 112, 213, 16, 213, 228, 243, 210, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< anInteger
	"Compare the receiver with the argument and return true if the receiver is
	less than the argument. Otherwise return false."

	anInteger isInteger
		ifTrue: [self negative == anInteger negative
					ifTrue: [self negative
								ifTrue: [^(self digitCompare: anInteger) > 0]
								ifFalse: [^(self digitCompare: anInteger) < 0]]
					ifFalse: [^self negative]]
		ifFalse: [^self retry: #< coercing: anInteger]

"""
    // 13 .. 46
    let expected = [16, 212, 172, 25, 112, 210, 16, 210, 198, 172, 15, 112, 210, 157, 112, 16, 227, 117, 179, 124, 112, 16, 227, 117, 178, 124, 112, 210, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testMod() throws {
    let source = """
\\\\ aNumber
	"Take the receiver modulo the argument and return the result.
	The result is the remainder rounded towards negative infinity, of the receiver
	divided by the argument. The remainder is defined in terms of //.
	The result has the same sign as the argument:
		e.g.  9\\4 = 1,  -9\\4 = 3,  9\\-4 =  -3,  -9\\-4 = -1 "

	^self - (self // aNumber * aNumber)

"""
    // 3 .. 10
    let expected = [112, 112, 16, 189, 16, 184, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= anInteger
	"Compare the receiver with the argument and return true if the receiver is
	less than or equal to the argument. Otherwise return false."

	^super <= anInteger

"""
    // 7 .. 11
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= anInteger
	"Compare the receiver with the argument and return true if the receiver is
	greater than or equal to the argument. Otherwise return false."

	^super >= anInteger

"""
    // 7 .. 11
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ aNumber
	"Divide the receiver by the argument and answer with the result if the division
	is exact."

	| quoRem |
	aNumber isInteger
		ifTrue:
			[quoRem _ self digitDiv: aNumber neg: self negative ~~ aNumber negative.
			(quoRem at: 2) = 0
				ifTrue: [^(quoRem at: 1) truncated]
				ifFalse: [^(Fraction numerator: self denominator: aNumber) reduced]]
		ifFalse: [^self retry: #/ coercing: aNumber]
"""
    // 23 .. 57
    let expected = [16, 217, 172, 26, 112, 16, 112, 212, 16, 212, 227, 242, 105, 17, 119, 192, 117, 182, 156, 17, 118, 192, 216, 124, 71, 112, 16, 246, 213, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> anInteger
	"Compare the receiver with the argument and return true if the receiver is
	greater than the argument. Otherwise return false."

	anInteger isInteger
		ifTrue: [self negative == anInteger negative
					ifTrue: [self negative
								ifTrue: [^(self digitCompare: anInteger) < 0]
								ifFalse: [^(self digitCompare: anInteger) > 0]]
					ifFalse: [^anInteger negative]]
		ifFalse: [^self retry: #> coercing: anInteger]

"""
    // 13 .. 46
    let expected = [16, 212, 172, 25, 112, 210, 16, 210, 198, 172, 15, 112, 210, 157, 112, 16, 227, 117, 178, 124, 112, 16, 227, 117, 179, 124, 16, 210, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testDiv() throws {
    let source = """
// aNumber
	"Divide the receiver by the argument and return the result.
	Round the result down towards negative infinity to make it a whole integer."

	| q |
	aNumber = 0 ifTrue: [^self error: 'division by 0'].
	self = 0 ifTrue: [^0].
	q _ self quo: aNumber.
	(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
		ifTrue: [^q - 1"Truncate towards minus infinity"]
		ifFalse: [^q]
"""
    // 11 .. 56
    let expected = [16, 117, 182, 155, 112, 33, 224, 124, 112, 117, 182, 153, 117, 124, 112, 16, 226, 105, 17, 211, 158, 17, 16, 184, 112, 183, 164, 11, 17, 117, 182, 157, 112, 211, 16, 211, 183, 144, 114, 155, 17, 118, 177, 124, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testPlus() throws {
    let source = """
+ aNumber
	"Add the receiver to the argument and answer with the result."

	aNumber isInteger
		ifTrue: [self negative == aNumber negative
					ifTrue: [^(self digitAdd: aNumber) truncated]
					ifFalse: [^self digitSubtract: aNumber]]
		ifFalse: [^self retry: #+ coercing: aNumber]

"""
    // 17 .. 40
    let expected = [16, 214, 172, 15, 112, 213, 16, 213, 198, 156, 112, 16, 228, 211, 124, 112, 16, 226, 124, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }



  func testPrintOnBase() throws {
    let source = """
printOn: aStream base: b
	"Print a representation of the receiver on the stream, aStream, in
	base b where 2<=b<=256."

	| digits source dest i j pos t rem |
	i _ self digitLength.
	"Estimate size of result, conservatively"
	digits _ Array new: i * 8.
	pos _ 0.
	dest _
		i <= 1
			ifTrue: [self]
			ifFalse: [LargePositiveInteger new: i].
	source _ self.
	[i > 1]
		whileTrue:
			[rem _ 0.
			j _ i.
			[j > 0]
				whileTrue:
					[t _ (rem bitShift: 8) + (source digitAt: j).
					dest digitAt: j put: t // b.
					rem _ t \\\\ b.
					j _ j - 1].
			pos _ pos + 1.
			digits at: pos put: rem.
			source _ dest.
			(source digitAt: i) = 0 ifTrue: [i _ i - 1]].
	(dest digitAt: 1) printOn: aStream base: b.
	[pos > 0]
		whileTrue:
			[aStream nextPut: (Character digitValue: (digits at: pos)).
			pos _ pos - 1]

"""
    // 21 .. 135
    let expected = [112, 208, 109, 65, 21, 34, 184, 205, 106, 117, 111, 21, 118, 180, 153, 112, 146, 67, 21, 205, 108, 112, 107, 21, 118, 179, 172, 60, 117, 130, 73, 21, 110, 22, 117, 179, 172, 27, 25, 34, 188, 19, 22, 228, 176, 130, 72, 20, 22, 24, 17, 189, 245, 135, 24, 17, 186, 130, 73, 22, 118, 177, 110, 163, 224, 23, 118, 176, 111, 18, 23, 25, 193, 135, 20, 107, 19, 21, 228, 117, 182, 155, 21, 118, 177, 109, 163, 191, 20, 118, 228, 16, 17, 246, 135, 23, 117, 179, 172, 14, 16, 72, 18, 23, 192, 231, 196, 135, 23, 118, 177, 111, 163, 237, 120]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= anInteger
	"Compare the receiver with the argument and return true if the receiver is
	equal to the argument. Otherwise return false."

	anInteger isInteger
		ifTrue: [anInteger positive & self positive | (anInteger negative & self negative)
					ifTrue: [^(self digitCompare: anInteger) = 0]
					ifFalse: [^false]]
		ifFalse: [^self retry: #= coercing: anInteger]

"""
    // 19 .. 46
    let expected = [16, 215, 172, 19, 16, 213, 112, 213, 228, 16, 214, 112, 214, 228, 227, 157, 112, 16, 226, 117, 182, 124, 122, 112, 33, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testGenerality() throws {
    let source = """
generality
	^40

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsInteger() throws {
    let source = """
isInteger
	^true

"""
    // 3 .. 3
    let expected = [121]
    try runningSource(source, expecting: expected)
  }

}
