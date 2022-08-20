import XCTest
@testable import CompilerLib

final class CompileCharacterTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Character", instanceVariables: ["value"])
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

  func testIsLowercase() throws {
    let source = """
isLowercase
	"Answer whether the receiver is a lowercase letter."
	^self >= $a and: [self <= $z]

"""
    // 7 .. 16
    let expected = [112, 33, 181, 155, 112, 32, 180, 144, 114, 124]
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

  func testIsDigit() throws {
    let source = """
isDigit
	"Answer whether the receiver is a digit."
	^self >= $0 and: [self <= $9]

"""
    // 7 .. 16
    let expected = [112, 33, 181, 155, 112, 32, 180, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsAlphaNumeric() throws {
    let source = """
isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."
	^self isLetter or: [self isDigit]

"""
    // 7 .. 14
    let expected = [112, 209, 153, 113, 145, 112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsLetter() throws {
    let source = """
isLetter
	"Answer whether the receiver is a letter."
	^(8r141 <= value and: [value <= 8r172])
		or: [8r101 <= value and: [value <= 8r132]]

"""
    // 11 .. 33
    let expected = [35, 0, 180, 155, 0, 34, 180, 144, 114, 154, 113, 164, 9, 33, 0, 180, 155, 0, 32, 180, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }


  func testAsUppercase() throws {
    let source = """
asUppercase
	"Answer a Character that is the upper case letter corresponding to the receiver.  If
	the receiver is not a lower case letter, answer the receiver itself."

	8r141 <= value
		ifTrue: [value <= 8r172
					ifTrue: [^Character value: value-8r40]]

"""
    // 11 .. 26
    let expected = [35, 0, 180, 172, 10, 0, 34, 180, 157, 64, 0, 33, 177, 202, 124, 120]
    try runningSource(source, expecting: expected)
  }



  func testEquals() throws {
    let source = """
= aCharacter
	"Answer true if the receiver and the argument are the same object (have the
	same object pointer) and false otherwise.  Optional.  See Object documentation
	whatIsAPrimitive."

	<primitive: 110>
	^self == aCharacter

"""
    // 7 .. 10
    let expected = [112, 16, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsSeparator() throws {
    let source = """
isSeparator
	"Answer whether the receiver is one of the separator characters--space,
	cr, tab, line feed, or form feed."

	value = 32 ifTrue: [^true].	"space"
	value = 13 ifTrue: [^true].	"cr"
	value = 9 ifTrue: [^true].	"tab"
	value = 10 ifTrue: [^true].	"line feed"
	value = 12 ifTrue: [^true].	"form feed"
	^false

"""
    // 13 .. 38
    let expected = [0, 32, 182, 152, 121, 0, 33, 182, 152, 121, 0, 34, 182, 152, 121, 0, 35, 182, 152, 121, 0, 36, 182, 152, 121, 122]
    try runningSource(source, expecting: expected)
  }


  func testDigitValue() throws {
    let source = """
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 otherwise.
	This is used to parse literal numbers of radix 2-36."

	value <= $9 asciiValue
		ifTrue: [^value - $0 asciiValue].
	value >= $A asciiValue
		ifTrue: [value <= $Z asciiValue ifTrue: [^value - $A asciiValue + 10]].
	^-1

"""
    // 15 .. 44
    let expected = [0, 34, 208, 180, 156, 0, 33, 208, 177, 124, 0, 35, 208, 181, 172, 12, 0, 37, 208, 180, 158, 0, 35, 208, 177, 36, 176, 124, 116, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"Character literals are preceded by '$'."
	aStream nextPut: $$; nextPut: self

"""
    // 5 .. 13
    let expected = [16, 136, 32, 196, 135, 112, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsSymbol() throws {
    let source = """
asSymbol
	^Symbol internCharacter: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testTokenish() throws {
    let source = """
tokenish
	"Answer whether the receiver is a valid token-character--letter, digit, or colon."

	^self isLetter or: [self isDigit or: [self = $:]]

"""
    // 9 .. 22
    let expected = [112, 210, 153, 113, 151, 112, 209, 153, 113, 146, 112, 32, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aCharacter
	"Answer true if the receiver's value < aCharacter's value."

	^self asciiValue < aCharacter asciiValue

"""
    // 5 .. 10
    let expected = [112, 208, 16, 208, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsUppercase() throws {
    let source = """
isUppercase
	"Answer whether the receiver is an uppercase letter."
	^self >= $A and: [self <= $Z]

"""
    // 7 .. 16
    let expected = [112, 33, 181, 155, 112, 32, 180, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPut: $$.
	aStream nextPut: self

"""
    // 5 .. 13
    let expected = [16, 32, 196, 135, 16, 112, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsLowercase() throws {
    let source = """
asLowercase
	"Answer a Character that is the lower case letter corresponding to the receiver.  If
	the receiver is not an upper case letter, answer the receiver itself."

	8r101 <= value
		ifTrue: [value <= 8r132
					ifTrue: [^Character value: value+8r40]]

"""
    // 11 .. 26
    let expected = [35, 0, 180, 172, 10, 0, 34, 180, 157, 64, 0, 33, 176, 202, 124, 120]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> aCharacter
	"Answer true if the receiver's value > aCharacter's value."

	^self asciiValue > aCharacter asciiValue

"""
    // 5 .. 10
    let expected = [112, 208, 16, 208, 179, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsVowel() throws {
    let source = """
isVowel
	"Answer whether the receiver is one of the vowels, AEIOU, in upper or lower case."

	^'AEIOU' includes: self asUppercase

"""
    // 9 .. 13
    let expected = [33, 112, 210, 224, 124]
    try runningSource(source, expecting: expected)
  }

}
