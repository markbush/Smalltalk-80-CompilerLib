import XCTest
@testable import CompilerLib

final class CompileStreamTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Stream", instanceVariables: [])
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

  func testContents() throws {
    let source = """
contents
	"Answer the contents of the receiver."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtEnd() throws {
    let source = """
atEnd
	"Answer whether the position is greater than or equal to the limit."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextN() throws {
    let source = """
next: anInteger
	"Answer an OrderedCollection of the next anInteger number of random numbers."
	| aCollection |
	aCollection _ OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection

"""
    // 9 .. 26
    let expected = [64, 204, 105, 16, 137, 117, 200, 164, 5, 17, 112, 195, 226, 125, 225, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	"Evaluate aBlock for each of the elements of the receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]

"""
    // 3 .. 14
    let expected = [112, 197, 168, 7, 16, 112, 195, 202, 135, 163, 245, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextPutAll() throws {
    let source = """
nextPutAll: aCollection
	"Append the elements of aCollection onto the receiver.  Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection

"""
    // 3 .. 17
    let expected = [16, 137, 118, 200, 164, 5, 105, 112, 17, 196, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testNextMatchFor() throws {
    let source = """
nextMatchFor: anObject
	"Gobble the next element and answer whether it is equal to anObject."

	^anObject = self next

"""
    // 3 .. 7
    let expected = [16, 112, 195, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testNextNPut() throws {
    let source = """
next: anInteger put: anObject
	"Put anObject into the next anInteger elements of the receiver."

	anInteger timesRepeat: [self nextPut: anObject].
	^anObject

"""
    // 5 .. 18
    let expected = [16, 137, 117, 200, 164, 4, 112, 17, 196, 125, 224, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testNext() throws {
    let source = """
next
	"Answer the next object in the receiver."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNextPut() throws {
    let source = """
nextPut: anObject
	"Insert the argument, anObject, at the next position in the receiver.
	Answer anObject."
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
