import XCTest
@testable import CompilerLib

final class CompileTextTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Text", instanceVariables: ["string", "runs"])
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

  func testEmphasizeFromToWith() throws {
    let source = """
emphasizeFrom: start to: stop with: emphasis
	"Set the emphasis for characters in the interval start-stop."

	runs _
		runs
			copyReplaceFrom: start
			to: stop
			with: (RunArray new: stop - start + 1 withAll: emphasis)

"""
    // 9 .. 23
    let expected = [1, 16, 17, 66, 17, 16, 177, 118, 176, 18, 241, 131, 96, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetStringSetRuns() throws {
    let source = """
setString: aString setRuns: anArray
	string _ aString.
	runs _ anArray

"""
    // 3 .. 7
    let expected = [16, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testAllBold() throws {
    let source = """
allBold
	self emphasizeFrom: 1 to: self size with: 2

"""
    // 5 .. 13
    let expected = [112, 118, 112, 194, 119, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	^self deepCopy

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }


  func testEmphasisAt() throws {
    let source = """
emphasisAt: characterIndex
	"Answer the code for characters in the run beginning at characterIndex."

	self size = 0 ifTrue: [^1].	"null text tolerates access"
	^runs at: characterIndex

"""
    // 3 .. 13
    let expected = [112, 194, 117, 182, 153, 118, 124, 1, 16, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= anotherText
	^string = anotherText string

"""
    // 5 .. 9
    let expected = [0, 16, 208, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsParagraph() throws {
    let source = """
asParagraph
	"Answer a Paragraph whose text is the receiver."
	^Paragraph withText: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }


  func testAsUppercase() throws {
    let source = """
asUppercase
	string _ string asUppercase

"""
    // 5 .. 8
    let expected = [0, 208, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testRunLengthFor() throws {
    let source = """
runLengthFor: characterIndex
	"Answer the count of characters remaining in run beginning with
	characterIndex."

	^runs runLengthAt: characterIndex

"""
    // 5 .. 8
    let expected = [1, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsNumber() throws {
    let source = """
asNumber
	"Answer the number created by interpreting the receiver as the textual
	representation of a number."

	^string asNumber

"""
    // 5 .. 7
    let expected = [0, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testReplaceFromToWith() throws {
    let source = """
replaceFrom: start to: stop with: aText
	string _ string copyReplaceFrom: start to: stop with: aText string.
	runs _ runs copyReplaceFrom: start to: stop with: aText runs

"""
    // 9 .. 25
    let expected = [0, 16, 17, 18, 209, 131, 96, 96, 1, 16, 17, 18, 210, 131, 96, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyFromTo() throws {
    let source = """
copyFrom: start to: stop
	"Answer with a copied subrange of this text"

	| realStart realStop |
	stop > self size
		ifTrue: [realStop _ self size]		"handle selection at end of string"
		ifFalse: [realStop _ stop].
	start < 1
		ifTrue: [realStart _ 1]			"handle selection before start of string"
		ifFalse: [realStart _ start].
	^Text
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)

"""
    // 9 .. 45
    let expected = [17, 112, 194, 179, 156, 112, 194, 129, 67, 146, 17, 129, 67, 135, 16, 118, 178, 155, 118, 129, 66, 146, 16, 129, 66, 135, 65, 0, 18, 19, 242, 1, 18, 19, 242, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyReplaceFromToWith() throws {
    let source = """
copyReplaceFrom: start to: stop with: aText
	^self shallowCopy replaceFrom: start to: stop with: aText

"""
    // 7 .. 14
    let expected = [112, 209, 16, 17, 18, 131, 96, 124]
    try runningSource(source, expecting: expected)
  }



  func testMakeSelectorBoldIn() throws {
    let source = """
makeSelectorBoldIn: aClass
	"For formatting Smalltalk source code, set the emphasis of that portion of
	the receiver's string that parses as a message selector to be bold."

	| parser |
	string size = 0 ifTrue: [^self].
	(parser _ aClass parserClass new) parseSelector: string.
	self emphasizeFrom: 1
		to: (parser endOfLastToken min: string size)
		with: 2

"""
    // 13 .. 38
    let expected = [0, 194, 117, 182, 152, 120, 16, 209, 204, 129, 65, 0, 224, 135, 112, 118, 17, 212, 0, 194, 227, 119, 131, 98, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsLowercase() throws {
    let source = """
asLowercase
	string _ string asLowercase

"""
    // 5 .. 8
    let expected = [0, 208, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)

"""
    // 13 .. 33
    let expected = [16, 136, 33, 224, 135, 136, 0, 226, 135, 136, 35, 224, 135, 136, 1, 226, 135, 36, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: index
	^string at: index

"""
    // 3 .. 6
    let expected = [0, 16, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: index put: character
	^string at: index put: character

"""
    // 3 .. 7
    let expected = [0, 16, 17, 193, 124]
    try runningSource(source, expecting: expected)
  }

  func testFindStringStartingAt() throws {
    let source = """
findString: aString startingAt: start
	"Answer the index of subString within the receiver, starting at position start.
	If the receiver does not contain subString, answer 0."

	^string findString: aString asString startingAt: start

"""
    // 7 .. 12
    let expected = [0, 16, 209, 17, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	^string size

"""
    // 3 .. 5
    let expected = [0, 194, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPutAll: 'Text for '.
	string printOn: aStream

"""
    // 9 .. 17
    let expected = [16, 33, 224, 135, 0, 16, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsDisplayText() throws {
    let source = """
asDisplayText
	"Answer a DisplayText whose text is the receiver."
	^DisplayText text: self

"""
    // 7 .. 10
    let expected = [65, 112, 224, 124]
    try runningSource(source, expecting: expected)
  }

}
