import XCTest
@testable import CompilerLib

final class CompileSharedQueueTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("SharedQueue", instanceVariables: ["contentsArray", "readPosition", "writePosition", "accessProtect", "readSynch"])
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

  func testMakeRoomAtEnd() throws {
    let source = """
makeRoomAtEnd
	| contentsSize |
	readPosition = 1
		ifTrue:
			[contentsArray grow]
		ifFalse:
			[contentsSize _ writePosition - readPosition.
			1 to: contentsSize do:
				[:index |
				contentsArray
					at: index
					put: (contentsArray at: index + readPosition - 1)].
			readPosition _ 1.
			writePosition _ contentsSize + 1]

"""
    // 7 .. 48
    let expected = [1, 118, 182, 155, 0, 209, 164, 32, 2, 1, 177, 104, 118, 16, 137, 118, 200, 164, 12, 105, 0, 17, 0, 17, 1, 176, 118, 177, 192, 193, 125, 240, 135, 118, 97, 16, 118, 176, 129, 2, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextPut() throws {
    let source = """
nextPut: value
	"Send value through the receiver.  If a Process has been suspended waiting to
	receive a value through the receiver, allow it to proceed."

	accessProtect
		critical: [writePosition > contentsArray size
						ifTrue: [self makeRoomAtEnd].
				 contentsArray at: writePosition put: value.
				 writePosition _ writePosition + 1].
	readSynch signal.
	^value

"""
    // 9 .. 40
    let expected = [3, 137, 117, 200, 164, 19, 2, 0, 194, 179, 154, 112, 209, 135, 0, 2, 16, 193, 135, 2, 118, 176, 129, 2, 125, 224, 135, 4, 210, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testInit() throws {
    let source = """
init: size
	contentsArray _ Array new: size.
	readPosition _ 1.
	writePosition _ 1.
	accessProtect _ Semaphore forMutualExclusion.
	readSynch _ Semaphore new

"""
    // 9 .. 23
    let expected = [64, 16, 205, 96, 118, 97, 118, 98, 66, 209, 99, 66, 204, 100, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsEmpty() throws {
    let source = """
isEmpty
	"Answer whether any objects have been sent through the receiver
	and not yet received by anyone."

	^readPosition = writePosition

"""
    // 3 .. 6
    let expected = [1, 2, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	"Answer the number of objects that have been sent through the
	receiver and not yet received by anyone."

	^writePosition - readPosition

"""
    // 3 .. 6
    let expected = [2, 1, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testNext() throws {
    let source = """
next
	"Answer the object that was sent through the receiver first and has not yet
	been received by anyone.  If no object has been sent, suspend the
	requesting process until one is."

	| value |
	readSynch wait.
	accessProtect
		critical: [readPosition = writePosition
					ifTrue:
						[self error: 'Error in SharedQueue synchronization'.
						 value _ nil]
					ifFalse:
						[value _ contentsArray at: readPosition.
						 contentsArray at: readPosition put: nil.
						 readPosition _ readPosition + 1]].
	^value

"""
    // 11 .. 52
    let expected = [4, 208, 135, 3, 137, 117, 200, 164, 29, 1, 2, 182, 172, 9, 112, 35, 226, 135, 115, 129, 64, 164, 14, 0, 1, 192, 104, 0, 1, 115, 193, 135, 1, 118, 176, 129, 1, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRelease() throws {
    let source = """
release
	contentsArray _ nil

"""
    // 3 .. 5
    let expected = [115, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testPeek() throws {
    let source = """
peek
	"Answer the object that was sent through the receiver first and has not yet
	been received by anyone but do not remove it from the receiver. If no object has
	been sent, suspend the requesting process until one is."

	| value |
	accessProtect
		critical: [readPosition >= writePosition
					ifTrue: [readPosition _ 1.
							writePosition _ 1.
							value _ nil]
					ifFalse: [value _ contentsArray at: readPosition]].
	^value

"""
    // 5 .. 32
    let expected = [3, 137, 117, 200, 164, 18, 1, 2, 181, 159, 118, 97, 118, 98, 115, 129, 64, 148, 0, 1, 192, 129, 64, 125, 224, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

}
