import XCTest
@testable import CompilerLib

final class CompileSymbolTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Symbol", instanceVariables: [])
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

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPutAll: self

"""
    // 5 .. 9
    let expected = [16, 112, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: anInteger put: anObject
	"you can not modify the receiver."

	self errorNoModification

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsString() throws {
    let source = """
asString
	| newString |
	newString _ String new: self size.
	1 to: self size do: [:index | newString at: index put: (self at: index)].
	^newString

"""
    // 7 .. 31
    let expected = [64, 112, 194, 205, 104, 118, 112, 194, 137, 118, 200, 164, 8, 105, 16, 17, 112, 17, 192, 193, 125, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testHashMappedBy() throws {
    let source = """
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map"
	^ map newHashFor: self hash

"""
    // 7 .. 11
    let expected = [16, 112, 209, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsInfix() throws {
    let source = """
isInfix
	"Answer whether the receiver is an infix message selector."
	^(self at: 1) isLetter not

"""
    // 7 .. 12
    let expected = [112, 118, 192, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsLiteral() throws {
    let source = """
isLiteral
	^Scanner isLiteralSymbol: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Answer with a SmallInteger whose value is half of the receiver's object pointer
	(interpreting object pointers as 16-bit signed quantities).  Essential.  See
	Object documentation whatIsAPrimitive."

	<primitive: 75>
	^self

"""
    // 7 .. 7
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testReplaceFromToWithStartingAt() throws {
    let source = """
replaceFrom: start to: stop with: replacement startingAt: repStart
	self errorNoModification

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testKeywords() throws {
    let source = """
keywords
	"Answer an array of the keywords that compose the receiver."

	| result aStream i l char |
	result _ WriteStream on: (Array new: 10).
	aStream _ WriteStream on: (String new: 16).
	i _ 1.
	l _ self size.
	[i <= l]
		whileTrue:
			[char _ self at: i.
			aStream nextPut: char.
			(char = $: or: [i = l])
				ifTrue:
					[result nextPut: aStream contents.
					aStream reset].
			i _ i + 1].
	^result contents

"""
    // 21 .. 77
    let expected = [65, 66, 35, 205, 224, 104, 65, 68, 37, 205, 224, 105, 118, 106, 112, 194, 107, 18, 19, 180, 172, 32, 112, 18, 192, 108, 17, 20, 196, 135, 20, 40, 182, 153, 113, 146, 18, 19, 182, 159, 16, 17, 214, 196, 135, 17, 215, 135, 18, 118, 176, 106, 163, 219, 16, 214, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsKeyword() throws {
    let source = """
isKeyword
	"Answer whether the receiver is a message keyword, i.e., ends with colon."

	self size <= 1 ifTrue: [^false].
	^(self at: self size) = $:

"""
    // 5 .. 17
    let expected = [112, 194, 118, 180, 152, 122, 112, 112, 194, 192, 32, 182, 124]
    try runningSource(source, expecting: expected)
  }


  func testEquals() throws {
    let source = """
= anObject
	^self == anObject

"""
    // 3 .. 6
    let expected = [112, 16, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testSelectorPart() throws {
    let source = """
selectorPart
	"return just my part after the class name if the receiver is a compound selector
	 (otherwise the whole thing)"
	^(self copyFrom: (self indexOf: $.) + 1 to: self size) asSymbol

"""
    // 11 .. 21
    let expected = [112, 112, 35, 226, 118, 176, 112, 194, 241, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsCompound() throws {
    let source = """
isCompound
	"return true if the receiver is of the form Class.foo "
	^self includes: $.

"""
    // 7 .. 10
    let expected = [112, 33, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorNoModification() throws {
    let source = """
errorNoModification
	self error:  'symbols can not be modified.'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNumArgs() throws {
    let source = """
numArgs
	"Answer the number of arguments that the receiver requires if it is interpreted
	as a message selector."

	| len n i |
	len _ self size.
	n _ (self at: 1) isLetter ifTrue: [0] ifFalse: [1].
	i _ 1.
	[(i _ i + 1) <= len]
		whileTrue: "count colons"
			[(self at: i) = $: ifTrue: [n _ n + 1]].
	^n

"""
    // 7 .. 43
    let expected = [112, 194, 104, 112, 118, 192, 208, 153, 117, 144, 118, 105, 118, 106, 18, 118, 176, 129, 66, 16, 180, 172, 12, 112, 18, 192, 33, 182, 155, 17, 118, 176, 105, 163, 235, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testClassPart() throws {
    let source = """
classPart
	"I must be a compound selector.  Return my class name"
	| i |
	i _ self indexOf: $. ifAbsent: [self error: 'class part not found'].
	^(self copyFrom: 1 to: i-1) asSymbol

"""
    // 15 .. 35
    let expected = [112, 33, 137, 117, 200, 164, 4, 112, 35, 226, 125, 240, 104, 112, 118, 16, 118, 177, 245, 212, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	self isLiteral
		ifTrue:
			[aStream nextPut: $#.
			aStream nextPutAll: self]
		ifFalse:
			[super storeOn: aStream.
			aStream nextPutAll: ' asSymbol']

"""
    // 15 .. 35
    let expected = [112, 212, 159, 16, 35, 196, 135, 16, 112, 225, 151, 112, 16, 133, 32, 135, 16, 34, 225, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testString() throws {
    let source = """
string: aString
	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self

"""
    // 9 .. 28
    let expected = [118, 16, 194, 137, 118, 200, 164, 9, 105, 112, 17, 16, 17, 192, 133, 65, 125, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testSpecies() throws {
    let source = """
species
	^String

"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testStringhash() throws {
    let source = """
stringhash
	^super hash

"""
    // 7 .. 10
    let expected = [112, 133, 0, 124]
    try runningSource(source, expecting: expected)
  }

}
