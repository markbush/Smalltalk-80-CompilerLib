import XCTest
@testable import CompilerLib

final class CompileReadWriteStreamTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("ReadWriteStream", instanceVariables: ["collection", "position", "readLimit", "writeLimit"])
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

  func testTimeStamp() throws {
    let source = """
timeStamp
	"Append the current time to the receiver as a chunk."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	Smalltalk timeStamp: aStream.
	self nextChunkPut: aStream contents printString.	"double quotes and !s"
	self cr; cr

"""
    // 23 .. 45
    let expected = [65, 66, 35, 205, 224, 104, 69, 16, 228, 135, 112, 16, 216, 215, 230, 135, 112, 136, 217, 135, 217, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testFileIn() throws {
    let source = """
fileIn
	"This is special for reading expressions from text that has been formatted
	with exclamation delimitors.  The expressions are read and passed to the
	Compiler.  Answer the result of compilation."

	| val |
	Cursor read showWhile:
		[[self atEnd]
			whileFalse:
				[self skipSeparators.
				val _ (self peekFor: $!)
							ifTrue: [(Compiler evaluate: self nextChunk logged: false)
									scanFrom: self]
							ifFalse: [Compiler evaluate: self nextChunk logged: true]].
		self close].
	^val

"""
    // 25 .. 65
    let expected = [66, 209, 137, 117, 200, 164, 30, 112, 197, 168, 23, 112, 211, 135, 112, 41, 232, 159, 69, 112, 214, 114, 244, 112, 231, 148, 69, 112, 214, 113, 244, 104, 163, 229, 112, 218, 125, 224, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testFileOutChanges() throws {
    let source = """
fileOutChanges
	"Append to the receiver a description of all system changes."

	Cursor write showWhile:
		[self timeStamp.
		Smalltalk changes fileOutOn: self.
		self close]

"""
    // 19 .. 39
    let expected = [66, 209, 137, 117, 200, 164, 11, 112, 211, 135, 70, 213, 112, 228, 135, 112, 215, 125, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutChangesFor() throws {
    let source = """
fileOutChangesFor: class
	"Append to the receiver a description of the changes to the argument, class."

	Cursor write showWhile:
		[self timeStamp.
		Smalltalk changes fileOutChangesFor: class on: self.
		self close]

"""
    // 19 .. 40
    let expected = [66, 209, 137, 117, 200, 164, 12, 112, 211, 135, 70, 213, 16, 112, 244, 135, 112, 215, 125, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNext() throws {
    let source = """
next
	"Return the next object in the Stream represented by the receiver. Fail if
	the collection of this stream is not an Array or a String. Fail if the
	stream is positioned at its end, or if the position is out of bounds in the
	collection. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 65>
	"treat me as a FIFO"
	position >= readLimit
		ifTrue: [^nil]
		ifFalse: [^collection at: (position _ position + 1)]

"""
    // 7 .. 19
    let expected = [1, 2, 181, 152, 123, 0, 1, 118, 176, 129, 1, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testContents() throws {
    let source = """
contents
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: readLimit

"""
    // 7 .. 15
    let expected = [2, 1, 224, 98, 0, 118, 2, 241, 124]
    try runningSource(source, expecting: expected)
  }

}
