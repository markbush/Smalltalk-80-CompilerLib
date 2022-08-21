import XCTest
@testable import CompilerLib

final class CompileStringTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("String", instanceVariables: [])
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


  func testBasicAt() throws {
    let source = """
basicAt: index
	"Answer the Character stored in the field of the receiver indexed by the
	argument.  Fail if the index argument is not an Integer or is out of bounds.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character value: (super at: index)

"""
    // 11 .. 17
    let expected = [64, 112, 16, 133, 33, 202, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsText() throws {
    let source = """
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	"Answer the number of indexable fields in the receiver.  This value is the
	same as the largest legal subscript.  Essential.  See Object documentation
	whatIsAPrimitive."

	<primitive: 62>
	^self basicSize

"""
    // 9 .. 11
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsLowercase() throws {
    let source = """
asLowercase
	"Answer a string made up from the receiver whose characters are all lowercase."

	| aStream |
	aStream _ WriteStream on: (String new: self size).
	self do: [:aCharacter | aStream nextPut: aCharacter asLowercase].
	^aStream contents

"""
    // 13 .. 36
    let expected = [65, 66, 112, 194, 205, 224, 104, 112, 137, 118, 200, 164, 6, 105, 16, 17, 211, 196, 125, 203, 135, 16, 212, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	"Print inside string quotes, doubling inbedded quotes."

	^self storeOn: aStream

"""
    // 5 .. 8
    let expected = [112, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aString
	"Answer true if and only if the receiver collates before aString.  The collation
	sequence is ascii with case differences ignored."

	^(self compare: aString) = 1

"""
    // 5 .. 10
    let expected = [112, 16, 224, 118, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsUppercase() throws {
    let source = """
asUppercase
	"Answer a string made up from the receiver whose characters are all uppercase."

	| aStream |
	aStream _ WriteStream on: (String new: self size).
	self do: [:aCharacter | aStream nextPut: aCharacter asUppercase].
	^aStream contents

"""
    // 13 .. 36
    let expected = [65, 66, 112, 194, 205, 224, 104, 112, 137, 118, 200, 164, 6, 105, 16, 17, 211, 196, 125, 203, 135, 16, 212, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= aString
	"Answer true if and only if the receiver collates before aString or is the
	same as aString.  The collation sequence is ascii with case differences ignored."

	^(self compare: aString) <= 2

"""
    // 5 .. 10
    let expected = [112, 16, 224, 119, 180, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= aString
	"Answer true if and only if the receiver collates after aString or is the
	same as aString.  The collation sequence is ascii with case differences ignored."

	^(self compare: aString) >= 2

"""
    // 5 .. 10
    let expected = [112, 16, 224, 119, 181, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> aString
	"Answer true if and only if the receiver collates after aString.  The collation
	sequence is ascii with case differences ignored."

	^(self compare: aString) = 3

"""
    // 7 .. 12
    let expected = [112, 16, 224, 33, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	| l m |
	(l _ m _ self size) <= 2
	  ifTrue:
		[l = 2
		  ifTrue: [m _ 3]
		  ifFalse:
			[l = 1
			  ifTrue: [^((self at: 1) asciiValue bitAnd: 127) * 106].
			^21845]].
	^(self at: 1) asciiValue * 48 + ((self at: (m - 1)) asciiValue + l)

"""
    // 15 .. 66
    let expected = [112, 194, 129, 65, 129, 64, 119, 180, 172, 26, 16, 119, 182, 156, 36, 129, 65, 164, 16, 16, 118, 182, 172, 9, 112, 118, 192, 208, 33, 190, 34, 184, 124, 35, 124, 135, 112, 118, 192, 208, 37, 184, 112, 17, 118, 177, 192, 208, 16, 176, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testStringhash() throws {
    let source = """
stringhash
	^self hash

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsDisplayText() throws {
    let source = """
asDisplayText
	"Answer a DisplayText whose text string is the receiver."
	^DisplayText text: self asText

"""
    // 9 .. 13
    let expected = [65, 112, 210, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSameAs() throws {
    let source = """
sameAs: aString
	"Answer whether the receiver collates precisely with aString. The collation
	sequence is ascii with case differences ignored."

	^(self compare: aString) = 2

"""
    // 5 .. 10
    let expected = [112, 16, 224, 119, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompare() throws {
    let source = """
compare: s
	| i len endResult u1 u2 mylen |
	mylen _ self size.
	len _ s size.
	mylen < len
		ifTrue:
			[len _ mylen.
			endResult _ 1]
		ifFalse: [endResult _ mylen = len
						ifTrue: [2]
						ifFalse: [3]].
	i _ 0.
	[(i _ i + 1) <= len]
		whileTrue:
			[u1 _ self at: i.
			u2 _ s at: i.
			u1 = u2
				ifFalse:
					[u1 _ u1 asUppercase.
					u2 _ u2 asUppercase.
					u1 = u2 ifFalse:
						[^u1 < u2
							ifTrue: [1]
							ifFalse: [3]]]].
	^endResult

"""
    // 7 .. 80
    let expected = [112, 194, 110, 16, 194, 106, 22, 18, 178, 158, 22, 106, 118, 129, 67, 164, 9, 22, 18, 182, 153, 119, 144, 32, 129, 67, 135, 117, 105, 17, 118, 176, 129, 65, 18, 180, 172, 34, 112, 17, 192, 108, 16, 17, 192, 109, 20, 21, 182, 168, 19, 20, 209, 108, 21, 209, 109, 20, 21, 182, 168, 8, 20, 21, 178, 153, 118, 144, 32, 124, 163, 213, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testDeepCopy() throws {
    let source = """
deepCopy
	"DeepCopy would otherwise mean make a copy of the character;  since
	characters are unique, just return a shallowCopy."

	^self shallowCopy

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsParagraph() throws {
    let source = """
asParagraph
	"Answer a Paragraph whose text string is the receiver."
	^Paragraph withText: self asText

"""
    // 9 .. 13
    let expected = [65, 112, 210, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsFileName() throws {
    let source = """
asFileName
	"Answer a string made up from the receiver that is an acceptable file name."
	^Disk checkName: self fixErrors: true

"""
    // 7 .. 11
    let expected = [65, 112, 113, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testBasicAtPut() throws {
    let source = """
basicAt: index put: aCharacter
	"Store the Character in the field of the receiver indicated by the index.  Fail if the
	index is not an Integer or is out of bounds, or if the argument is not a Character.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 64>
	(aCharacter isKindOf: Character)
		ifTrue: [self errorNonIntegerIndex]
		ifFalse: [self error: 'Strings only store Characters']

"""
    // 17 .. 28
    let expected = [17, 68, 227, 154, 112, 210, 146, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"Print inside string quotes, doubling inbedded quotes."

	| i length x |
	aStream nextPut: $'.
	i _ 0.
	length _ self size.
	[(i _ i + 1) <= length]
		whileTrue:
			[aStream nextPut: (x _ self at: i).
			x == $' ifTrue: [aStream nextPut: x]].
	"embedded quotes get doubled"
	aStream nextPut: $'

"""
    // 5 .. 45
    let expected = [16, 32, 196, 135, 117, 105, 112, 194, 106, 17, 118, 176, 129, 65, 18, 180, 172, 18, 16, 112, 17, 192, 129, 67, 196, 135, 19, 32, 198, 155, 16, 19, 196, 135, 163, 229, 16, 32, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOldRunDecodeOn() throws {
    let source = """
oldRunDecodeOn: decodedStream
	"Decodes strings encoded by the message oldRunEncoded.
	Output is written onto decodedStream"
	| index size byte count bitsValue |
	index _ 0. size _ self size.
	[index>=size] whileFalse:
		[byte _ (self at: (index _ index+1)).
		byte asInteger == 0
			ifTrue:
				[count _ (self at: (index _ index+1)) asInteger.
				count = 0
					ifTrue:  "<0> <0> means one zero byte"
						[decodedStream nextPut: byte]
					ifFalse:  "<0> <count> <bitsValue> means count bytes = bitsValue"
						[bitsValue _ (self at: (index _ index+1)).
						[(count _ count-1)>=0] whileTrue: [decodedStream nextPut: bitsValue]]]
			ifFalse:  "<nonZero> means one nonZero byte"
				[decodedStream nextPut: byte]].
	^ decodedStream contents

"""
    // 7 .. 81
    let expected = [117, 105, 112, 194, 106, 17, 18, 181, 168, 62, 112, 17, 118, 176, 129, 65, 192, 107, 19, 208, 117, 198, 172, 42, 112, 17, 118, 176, 129, 65, 192, 208, 108, 20, 117, 182, 156, 16, 19, 196, 164, 23, 112, 17, 118, 176, 129, 65, 192, 109, 20, 118, 177, 129, 68, 117, 181, 157, 16, 21, 196, 135, 163, 242, 115, 146, 16, 19, 196, 135, 163, 189, 16, 209, 124]
    try runningSource(source, expecting: expected)
  }

  func testFindStringStartingAt() throws {
    let source = """
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If the receiver does not contain subString, answer 0."

	| aCharacter index |
	subString isEmpty ifTrue: [^0].
	aCharacter _ subString first.
	start to: self size - subString size + 1 do:
		[:startIndex |
		(self at: startIndex) = aCharacter ifTrue:
			[index _ 1.
			[(self at: startIndex+index-1) = (subString at: index)] whileTrue:
				[index = subString size ifTrue: [^startIndex].
				index _ index+1]]].
	^0

"""
    // 9 .. 73
    let expected = [16, 208, 153, 117, 124, 16, 209, 106, 17, 112, 194, 16, 194, 177, 118, 176, 137, 118, 200, 164, 40, 108, 112, 20, 192, 18, 182, 172, 30, 118, 107, 112, 20, 19, 176, 118, 177, 192, 16, 19, 192, 182, 172, 13, 19, 16, 194, 182, 153, 20, 124, 19, 118, 176, 107, 163, 230, 115, 144, 115, 125, 242, 135, 117, 124]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: index
	"Answer the Character stored in the field of the receiver indexed by the
	argument.  Fail if the index argument is not an Integer or is out of bounds.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character value: (super at: index)

"""
    // 11 .. 17
    let expected = [64, 112, 16, 133, 33, 202, 124]
    try runningSource(source, expecting: expected)
  }

  func testDisplayAt() throws {
    let source = """
displayAt: aPoint
	"Show a representation of the receiver as a DisplayText at location
	aPoint on the display screen."
	self asDisplayText displayAt: aPoint

"""
    // 7 .. 12
    let expected = [112, 209, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: index put: aCharacter
	"Store the Character in the field of the receiver indicated by the index.  Fail if the
	index is not an Integer or is out of bounds, or if the argument is not a Character.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 64>
	(aCharacter isKindOf: Character)
		ifTrue: [self errorNonIntegerIndex]
		ifFalse: [self error: 'Strings only store Characters']

"""
    // 17 .. 28
    let expected = [17, 68, 227, 154, 112, 210, 146, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testWithCRs() throws {
    let source = """
withCRs
	"substitute CRs for backslashes"
	^ self collect: [:char | char = $\\ ifTrue: [Character cr] ifFalse: [char]]

"""
    // 11 .. 28
    let expected = [112, 137, 118, 200, 164, 10, 104, 16, 35, 182, 154, 66, 209, 144, 16, 125, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrimReplaceFromToWithStartingAt() throws {
    let source = """
primReplaceFrom: start to: stop with: replacement startingAt: repStart
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart

"""
    // 9 .. 17
    let expected = [112, 16, 17, 18, 19, 133, 128, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHashMappedBy() throws {
    let source = """
hashMappedBy: map
	"My hash is independent of my oop"
	^ self hash

"""
    // 5 .. 7
    let expected = [112, 208, 124]
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

  func testReplaceFromToWithStartingAt() throws {
    let source = """
replaceFrom: start to: stop with: replacement startingAt: repStart
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver."
	(replacement isKindOf: String)
		ifTrue:
			[self primReplaceFrom: start to: stop with: replacement startingAt: repStart]
		ifFalse:
			[super replaceFrom: start to: stop with: replacement startingAt: repStart]

"""
    // 13 .. 33
    let expected = [18, 67, 226, 159, 112, 16, 17, 18, 19, 131, 129, 150, 112, 16, 17, 18, 19, 133, 128, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testContractTo() throws {
    let source = """
contractTo: charCount  "Shorten by ellipsis if too long"
	| half |
	self size > charCount ifTrue:
		[half _ charCount // 2.
		^ self copyReplaceFrom: half
				to: self size - (charCount-half) + 2
				with: '...']
	"
	'antidisestablishmentarianism' contractTo: 10 'anti...ism'
	"

"""
    // 7 .. 31
    let expected = [112, 194, 16, 179, 172, 18, 16, 119, 189, 105, 112, 17, 112, 194, 16, 17, 177, 177, 119, 176, 33, 131, 96, 124, 120]
    try runningSource(source, expecting: expected)
  }

  func testReplaceFromToWithByteArrayStartingAt() throws {
    let source = """
replaceFrom: start to: stop withByteArray: aByteArray startingAt: repStart
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the byte array, aByteArray.  Answer the
	receiver."
	| index repOff characterTable |
	<primitive: 105>
	repOff _ repStart - start.
	characterTable _ Character characterTable.	"in-line asCharacter for speed"
	index _ start - 1.
	[(index _ index + 1) <= stop]
		whileTrue:
			[self at: index put: (characterTable at: (aByteArray at: repOff + index)+1)]

"""
    // 11 .. 46
    let expected = [19, 16, 177, 109, 65, 208, 110, 16, 118, 177, 108, 20, 118, 176, 129, 68, 17, 180, 172, 15, 112, 20, 22, 18, 21, 20, 176, 192, 118, 176, 192, 193, 135, 163, 232, 120]
    try runningSource(source, expecting: expected)
  }

  func testDisplayOnAt() throws {
    let source = """
displayOn: aDisplayMedium at: aPoint
	"Show a representation of the receiver as a DisplayText at location
	aPoint on aDisplayMedium."
	self asDisplayText displayOn: aDisplayMedium at: aPoint

"""
    // 7 .. 13
    let expected = [112, 209, 16, 17, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsSymbol() throws {
    let source = """
asSymbol
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testOldRunEncoded() throws {
    let source = """
oldRunEncoded
	"Returns a string with equal consecutive bytes encoded as
		<0> <count> <val>
	Single zeroes are encoded as <0> <0> "

	| stream count previousByte byte |
	stream _ WriteStream on: (String new: self size).
	count _ 0.
	previousByte _ self at: 1.
	2 to: self size do:
		[:i | byte _ self at: i.
		(byte = previousByte and: [count < 255])
			ifTrue: [count _ count + 1]
			ifFalse:
				[count > 0
					ifTrue:
						[stream nextPut: (Character value: 0);
							nextPut: (Character value: count+1);
							nextPut: previousByte.
						count _ 0]
					ifFalse:
						[stream nextPut: previousByte.
						previousByte asInteger = 0
							ifTrue: [stream nextPut: previousByte]].
				previousByte _ byte]].
	count > 0
		ifTrue:
			[stream nextPut: (Character value: 0);
				nextPut: (Character value: count+1);
				nextPut: previousByte]
		ifFalse:
			[stream nextPut: previousByte.
			previousByte asInteger = 0 ifTrue: [stream nextPut: previousByte]].
	^stream contents

"""
    // 19 .. 152
    let expected = [65, 66, 112, 194, 205, 224, 104, 117, 105, 112, 118, 192, 106, 119, 112, 194, 137, 118, 200, 164, 69, 108, 112, 20, 192, 107, 19, 18, 182, 155, 17, 38, 178, 144, 114, 158, 17, 118, 176, 129, 65, 164, 46, 17, 117, 179, 172, 23, 16, 136, 69, 117, 202, 196, 135, 136, 69, 17, 118, 176, 202, 196, 135, 18, 196, 135, 117, 129, 65, 164, 14, 16, 18, 196, 135, 18, 212, 117, 182, 155, 16, 18, 196, 144, 115, 135, 19, 129, 66, 125, 243, 135, 17, 117, 179, 172, 19, 16, 136, 69, 117, 202, 196, 135, 136, 69, 17, 118, 176, 202, 196, 135, 18, 196, 164, 14, 16, 18, 196, 135, 18, 212, 117, 182, 155, 16, 18, 196, 144, 115, 135, 16, 215, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsNumber() throws {
    let source = """
asNumber
	"Answer the number created by interpreting the receiver as the string
	representation of a number."

	^Number readFromString: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSpellAgainst() throws {
    let source = """
spellAgainst: aString
	"Answer an integer between 0 and 100 indicating how similar the argument is to the receiver.  No case conversion is done."

	| i1 i2 size1 size2 score maxLen |
	size1 _ self size.
	size2 _ aString size.
	maxLen _ size1 max: size2.
	score _ 0.
	i1 _ i2 _ 1.
	[i1 <= size1 and: [i2 <= size2]] whileTrue:
		[(self at: i1) = (aString at: i2)
			ifTrue: [score _ score+1. 		"match"
					i1 _ i1+1. 				"advance both"
					i2 _ i2+1]
			ifFalse: [(i2 < size2 and: [(self at: i1) = (aString at: i2+1)])
						ifTrue: [i2 _ i2+1] 	"skip in other"
			ifFalse: [(i1 < size1 and: [(self at: i1+1) = (aString at: i2)])
						ifTrue: [i1 _ i1+1] 	"skip in self"
			ifFalse: [i1 _ i1+1. 				"miss - advance both"
					i2 _ i2+1] ] ] ].

	score = maxLen
		ifTrue: [^100]
		ifFalse: [^100*score//maxLen]

	" 'Smalltalk' spellAgainst: 'Smalltlak' "

"""
    // 7 .. 129
    let expected = [112, 194, 107, 16, 194, 108, 19, 20, 224, 110, 117, 109, 118, 129, 66, 105, 17, 19, 180, 155, 18, 20, 180, 144, 114, 172, 84, 112, 17, 192, 16, 18, 192, 182, 172, 15, 21, 118, 176, 109, 17, 118, 176, 105, 18, 118, 176, 129, 66, 164, 57, 18, 20, 178, 172, 10, 112, 17, 192, 16, 18, 118, 176, 192, 182, 144, 114, 158, 18, 118, 176, 129, 66, 164, 33, 17, 19, 178, 172, 10, 112, 17, 118, 176, 192, 16, 18, 192, 182, 144, 114, 158, 17, 118, 176, 129, 65, 164, 9, 17, 118, 176, 105, 18, 118, 176, 129, 66, 135, 163, 161, 21, 22, 182, 153, 33, 124, 33, 21, 184, 22, 189, 124]
    try runningSource(source, expecting: expected)
  }

  func testMatch() throws {
    let source = """
match: text
	"Answer whether text matches the pattern in the receiver.  Matching
	ignores upper/lower case differences.  Where the receiver contains #, text may
	contain any single character.  Where the receiver contains *, text may contain any
	sequence of characters."

	| pattern scanning p t back textStream startScan |
	pattern _ ReadStream on: self.
	textStream _ ReadStream on: text.
	scanning _ false.
	[pattern atEnd]
		whileFalse:
			[p _ pattern next.
			p = $*
				ifTrue:
					[pattern atEnd ifTrue: [^true].
					scanning _ true.
					startScan _ pattern position]
				ifFalse:
					[textStream atEnd ifTrue: [^false].
					t _ textStream next.
					(t asUppercase = p asUppercase or: [p = $#])
						ifFalse:
							[scanning ifFalse: [^false].
							back _ startScan - pattern position.
							pattern skip: back.
							textStream skip: back + 1]].
			(scanning and: [pattern atEnd and: [textStream atEnd not]])
				ifTrue: [back _ startScan - pattern position.
						pattern skip: back.
						textStream skip: back + 1]
			].
	^textStream atEnd

	" Examples:

	'xyz' match: 'Xyz'  true
	'x#z' match: 'x@z' true
	'x*z' match: 'x whyNot? z' true
	'*x' match: 'xx' true
	"

"""
    // 19 .. 128
    let expected = [65, 112, 224, 105, 65, 16, 224, 110, 114, 106, 17, 197, 168, 93, 17, 195, 107, 19, 38, 182, 172, 12, 17, 197, 152, 121, 113, 106, 17, 210, 129, 71, 164, 40, 22, 197, 152, 122, 22, 195, 108, 20, 213, 19, 213, 182, 153, 113, 146, 19, 36, 182, 154, 115, 164, 18, 18, 168, 1, 122, 23, 17, 210, 177, 109, 17, 21, 227, 135, 22, 21, 118, 176, 227, 135, 18, 172, 9, 17, 197, 155, 22, 197, 215, 144, 114, 144, 114, 172, 15, 23, 17, 210, 177, 109, 17, 21, 227, 135, 22, 21, 118, 176, 227, 135, 163, 159, 22, 197, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyUpTo() throws {
    let source = """
copyUpTo: aCharacter
	"Answer a copy of the receiver from index 1 to the first occurrence of
	aCharacter, non-inclusive."

	| index |
	index _ self indexOf: aCharacter ifAbsent: [^self].
	^self copyFrom: 1 to: index-1

"""
    // 7 .. 23
    let expected = [112, 16, 137, 117, 200, 164, 1, 120, 240, 105, 112, 118, 17, 118, 177, 241, 124]
    try runningSource(source, expecting: expected)
  }

}
