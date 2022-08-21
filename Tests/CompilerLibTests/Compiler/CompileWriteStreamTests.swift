import XCTest
@testable import CompilerLib

final class CompileWriteStreamTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("WriteStream", instanceVariables: ["collection", "position", "readLimit", "writeLimit"])
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

  func testOn() throws {
    let source = """
on: aCollection
	super on: aCollection.
	readLimit _ 0.
	writeLimit _ aCollection size

"""
    // 7 .. 17
    let expected = [112, 16, 133, 32, 135, 117, 98, 16, 194, 99, 120]
    try runningSource(source, expecting: expected)
  }

  func testPastEndPut() throws {
    let source = """
pastEndPut: anObject
	collection grow.
	writeLimit _ collection size.
	collection at: (position _ position + 1) put: anObject

"""
    // 5 .. 20
    let expected = [0, 208, 135, 0, 194, 99, 0, 1, 118, 176, 129, 1, 16, 193, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCr() throws {
    let source = """
cr
	"Append a return character to the receiver."

	self nextPut: Character cr

"""
    // 7 .. 12
    let expected = [112, 65, 208, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testWith() throws {
    let source = """
with: aCollection
	super on: aCollection.
	position _ readLimit _ writeLimit _ aCollection size

"""
    // 7 .. 19
    let expected = [112, 16, 133, 32, 135, 16, 194, 129, 3, 129, 2, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testEmphasis() throws {
    let source = """
emphasis
	^ 1
	"Allows compatibility with streams which carry emphasis"

"""
    // 3 .. 4
    let expected = [118, 124]
    try runningSource(source, expecting: expected)
  }

  func testPosition() throws {
    let source = """
position: anInteger
	readLimit _ readLimit max: position.
	super position: anInteger

"""
    // 9 .. 18
    let expected = [2, 1, 224, 98, 112, 16, 133, 33, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetEmphasis() throws {
    let source = """
emphasis: ignored
	"Allows compatibility with streams which carry emphasis"

"""
    // 3 .. 3
    let expected = [120]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	^readLimit _ readLimit max: position

"""
    // 5 .. 10
    let expected = [2, 1, 224, 129, 2, 124]
    try runningSource(source, expecting: expected)
  }

  func testReset() throws {
    let source = """
reset
	readLimit _ readLimit max: position.
	position _ 0

"""
    // 5 .. 11
    let expected = [2, 1, 224, 98, 117, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testTab() throws {
    let source = """
tab
	"Append a tab character to the receiver."

	self nextPut: Character tab

"""
    // 7 .. 12
    let expected = [112, 65, 208, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextChunkPut() throws {
    let source = """
nextChunkPut: aString
	"Put aString onto self, doubling embedded terminators."

	| index stringSize char terminator |
	terminator _ $!.
	index _ 0.
	stringSize _ aString size.
	[(index _ index + 1) <= stringSize]
		whileTrue:
			[char _ aString at: index.
			self nextPut: char.
			char == terminator ifTrue: ["double imbedded terminator"
				self nextPut: char]].
	self nextPut: terminator

"""
    // 5 .. 43
    let expected = [32, 108, 117, 105, 16, 194, 106, 17, 118, 176, 129, 65, 18, 180, 172, 18, 16, 17, 192, 107, 112, 19, 196, 135, 19, 20, 198, 155, 112, 19, 196, 135, 163, 229, 112, 20, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextPut() throws {
    let source = """
nextPut: anObject
	"Insert the argument at the next position in the Stream represented by the
	receiver. Fail if the collection of this stream is not an Array or a String.
	Fail if the stream is positioned at its end, or if the position is out of
	bounds in the collection. Fail if the argument is not of the right type for
	the collection. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 66>
	position = writeLimit
		ifTrue: [^self pastEndPut: anObject]
		ifFalse:
			[^collection at: (position _ position + 1) put: anObject]

"""
    // 9 .. 25
    let expected = [1, 3, 182, 155, 112, 16, 224, 124, 0, 1, 118, 176, 129, 1, 16, 193, 124]
    try runningSource(source, expecting: expected)
  }

  func testContents() throws {
    let source = """
contents
	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: position

"""
    // 7 .. 15
    let expected = [2, 1, 224, 98, 0, 118, 1, 241, 124]
    try runningSource(source, expecting: expected)
  }

  func testSpace() throws {
    let source = """
space
	"Append a space character to the receiver."

	self nextPut: Character space

"""
    // 7 .. 12
    let expected = [112, 65, 208, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCrtab() throws {
    let source = """
crtab
	"Append a return character, followed by a single tab character, to the receiver."

	self nextPut: Character cr.
	self nextPut: Character tab

"""
    // 9 .. 19
    let expected = [112, 65, 208, 196, 135, 112, 65, 210, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNext() throws {
    let source = """
next
	self shouldNotImplement

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetCrtab() throws {
    let source = """
crtab: anInteger
	"Append a return character, followed by anInteger tab characters, to the receiver."

	self nextPut: Character cr.
	anInteger timesRepeat: [self nextPut: Character tab]

"""
    // 11 .. 29
    let expected = [112, 65, 208, 196, 135, 16, 137, 117, 200, 164, 5, 112, 65, 211, 196, 125, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOnFromTo() throws {
    let source = """
on: aCollection from: firstIndex to: lastIndex
	| len |
	collection _ aCollection.
	readLimit _
		writeLimit _ lastIndex > (len _ collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position _ firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]

"""
    // 3 .. 28
    let expected = [16, 96, 18, 0, 194, 129, 67, 179, 153, 19, 144, 18, 129, 3, 98, 17, 118, 180, 153, 117, 146, 17, 118, 177, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrint() throws {
    let source = """
print: anObject
	"Have anObject print on the receiver."

	anObject printOn: self

"""
    // 5 .. 9
    let expected = [16, 112, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testStore() throws {
    let source = """
store: anObject
	"Have anObject print on me for rereading."

	anObject storeOn: self

"""
    // 5 .. 9
    let expected = [16, 112, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
