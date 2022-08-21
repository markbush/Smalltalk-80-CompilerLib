import XCTest
@testable import CompilerLib

final class CompilePositionableStreamTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("PositionableStream", instanceVariables: ["collection", "position", "readLimit"])
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

  func testPosition() throws {
    let source = """
position: anInteger
	"Set position to anInteger as long as anInteger is within the bounds of the
	receiver's contents.  If it is not, cause an error."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position _ anInteger]
		ifFalse: [self positionError]

"""
    // 7 .. 22
    let expected = [16, 117, 181, 16, 2, 180, 225, 155, 16, 129, 1, 145, 112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testContents() throws {
    let source = """
contents
	"Answer with a copy of my collection from 1 to readLimit."

	^collection copyFrom: 1 to: readLimit

"""
    // 5 .. 9
    let expected = [0, 118, 2, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtEnd() throws {
    let source = """
atEnd
	"Answer true if the position is greater than or equal to the limit,
	otherwise answer false. Fail if position or readLimit is not a SmallInteger.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 67>
	^position >= readLimit

"""
    // 7 .. 10
    let expected = [1, 2, 181, 124]
    try runningSource(source, expecting: expected)
  }

  func testNext() throws {
    let source = """
next: anInteger
	"Answer the next anInteger elements of the receiver."
	| newArray |
	newArray _ self contents species new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray

"""
    // 9 .. 32
    let expected = [112, 209, 208, 16, 205, 105, 118, 16, 137, 118, 200, 164, 7, 106, 17, 18, 112, 195, 193, 125, 242, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }


  func testThrough() throws {
    let source = """
through: anObject
	"Answer a subcollection from position to the occurrence (if any, not
	inclusive) of anObject. If not there, answer everything."

	| newStream element |
	newStream _ WriteStream on: (collection species new: 64).
	[(self atEnd or: [(element _ self next) = anObject]) or: [newStream size > 64000]]
		whileFalse: [newStream nextPut: element].
	self atEnd
		ifFalse:	[newStream nextPut: element].
	^newStream contents

"""
    // 15 .. 58
    let expected = [65, 0, 210, 35, 205, 224, 105, 112, 197, 153, 113, 149, 112, 195, 129, 66, 16, 182, 153, 113, 147, 17, 194, 36, 179, 168, 6, 17, 18, 196, 135, 163, 230, 112, 197, 168, 4, 17, 18, 196, 135, 17, 213, 124]
    try runningSource(source, expecting: expected)
  }

  func testPeek() throws {
    let source = """
peek
	"Answer what would be returned with a self next, without
	changing position.  If the receiver is at the end, answer nil."

	| nextObject |
	self atEnd ifTrue: [^nil].
	nextObject _ self next.
	position _ position - 1.
	^nextObject

"""
    // 3 .. 15
    let expected = [112, 197, 152, 123, 112, 195, 104, 1, 118, 177, 97, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsEmpty() throws {
    let source = """
isEmpty
	"Answer whether the receiver has no elements."
	^position = 0

"""
    // 3 .. 6
    let expected = [1, 117, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testOn() throws {
    let source = """
on: aCollection
	collection _ aCollection.
	readLimit _ aCollection size.
	position _ 0.
	self reset

"""
    // 5 .. 15
    let expected = [16, 96, 16, 194, 98, 117, 97, 112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReset() throws {
    let source = """
reset
	"Set the receiver's position to 0."
	position _ 0

"""
    // 3 .. 5
    let expected = [117, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testPeekFor() throws {
    let source = """
peekFor: anObject
	"Answer false and do not move the position if self next ~= anObject or if the
	receiver is at the end. Answer true and increment position if self next = anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject _ self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position _ position - 1.
	^false

"""
    // 3 .. 19
    let expected = [112, 197, 152, 122, 112, 195, 105, 16, 17, 182, 152, 121, 1, 118, 177, 97, 122]
    try runningSource(source, expecting: expected)
  }

  func testSkip() throws {
    let source = """
skip: anInteger
	"Set position to position+anInteger. A subclass might choose to be more
	helpful and select the minimum of self size and position+anInteger or
	maximum of 1 and position+anInteger for the repositioning."

	self position: position + anInteger

"""
    // 5 .. 11
    let expected = [112, 1, 16, 176, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSkipTo() throws {
    let source = """
skipTo: anObject
	"Position the receiver past the next occurrance of anObject.  Answer true if
	anObject is found, false otherwise."

	[self atEnd]
		whileFalse: [self next = anObject ifTrue: [^true]].
	^false

"""
    // 3 .. 15
    let expected = [112, 197, 168, 8, 112, 195, 16, 182, 152, 121, 163, 244, 122]
    try runningSource(source, expecting: expected)
  }

  func testSkipSeparators() throws {
    let source = """
skipSeparators
	"Move the receiver's position past any separators."

	[self atEnd == false and: [self peek isSeparator]]
		whileTrue: [self next]

"""
    // 7 .. 23
    let expected = [112, 197, 114, 198, 155, 112, 209, 208, 144, 114, 156, 112, 195, 135, 163, 240, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetToEnd() throws {
    let source = """
setToEnd
	"Set the position of the receiver to the end of its stream of elements."
	position _ readLimit

"""
    // 3 .. 5
    let expected = [2, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testPositionError() throws {
    let source = """
positionError
	"Since I am not necessarily writable, it is up to my subclasses to override
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testUpTo() throws {
    let source = """
upTo: anObject
	"Answer a subcollection from position to the occurrence (if any, not
	inclusive) of anObject. If not there, answer everything."

	| newStream element |
	newStream _ WriteStream on: (collection species new: 64).
	[self atEnd or: [(element _ self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents

"""
    // 13 .. 41
    let expected = [65, 0, 210, 35, 205, 224, 105, 112, 197, 153, 113, 149, 112, 195, 129, 66, 16, 182, 168, 6, 17, 18, 196, 135, 163, 237, 17, 212, 124]
    try runningSource(source, expecting: expected)
  }

  func testNextChunk() throws {
    let source = """
nextChunk
	"Answer the contents of the receiver, up to the next terminator character, with
	double terminators ignored."

	| aStream char terminator |
	terminator _ $!.
	aStream _ WriteStream on: (String new: 200).
	self skipSeparators.
	[(char _ self next)==nil]
		whileFalse:
			[char == terminator
				ifTrue: [(self peekFor: terminator)
						ifTrue: ["doubled terminator"
							aStream nextPut: char]
						ifFalse: [^aStream contents]]
				ifFalse: [aStream nextPut: char]].
	^aStream contents

"""
    // 19 .. 63
    let expected = [32, 106, 66, 67, 36, 205, 225, 104, 112, 213, 135, 112, 195, 129, 65, 115, 198, 168, 23, 17, 18, 198, 172, 12, 112, 18, 231, 155, 16, 17, 196, 146, 16, 214, 124, 146, 16, 17, 196, 135, 163, 225, 16, 214, 124]
    try runningSource(source, expecting: expected)
  }

}
