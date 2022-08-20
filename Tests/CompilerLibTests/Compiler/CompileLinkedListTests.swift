import XCTest
@testable import CompilerLib

final class CompileLinkedListTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("LinkedList", instanceVariables: ["firstLink", "lastLink"])
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

  func testRemoveLast() throws {
    let source = """
removeLast
	"Remove the receiver's last element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using addLast:/removeLast causes the
	receiver to behave as a stack; using addFirst:/removeLast causes the receiver to
	behave as a queue."

	| oldLink aLink |
	self emptyCheck.
	oldLink _ lastLink.
	firstLink == lastLink
		ifTrue: [firstLink _ nil. lastLink _ nil]
		ifFalse: [aLink _ firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink _ aLink nextLink].
				 aLink nextLink: nil.
				 lastLink _ aLink].
	oldLink nextLink: nil.
	^oldLink
"""
    // 9 .. 51
    let expected = [112, 208, 135, 1, 104, 0, 1, 198, 158, 115, 96, 115, 129, 1, 164, 20, 0, 105, 17, 209, 16, 198, 168, 5, 17, 209, 105, 163, 245, 17, 115, 226, 135, 17, 129, 1, 135, 16, 115, 226, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	"Answer how many elements the receiver contains."
	| tally |
	tally _ 0.
	self do: [:each | tally _ tally + 1].
	^tally
"""
    // 3 .. 21
    let expected = [117, 104, 112, 137, 118, 200, 164, 7, 105, 16, 118, 176, 129, 64, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveFirst() throws {
    let source = """
removeFirst
	"Remove the first element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using the sequence addFirst:/removeFirst
	causes the receiver to behave as a stack; using addLast:/removeFirst causes the
	receiver to behave as a queue."

	| oldLink |
	self emptyCheck.
	oldLink _ firstLink.
	firstLink == lastLink
		ifTrue: [firstLink _ nil. lastLink _ nil]
		ifFalse: [firstLink _ oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink
"""
    // 9 .. 34
    let expected = [112, 208, 135, 0, 104, 0, 1, 198, 157, 115, 96, 115, 129, 1, 147, 16, 209, 129, 0, 135, 16, 115, 226, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: aLink
	"Add aLink to the end of the receiver's list."

	^self addLast: aLink
"""
    // 5 .. 8
    let expected = [112, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	| aLink |
	aLink _ firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink _ aLink nextLink]
"""
    // 5 .. 21
    let expected = [0, 105, 17, 115, 198, 168, 9, 16, 17, 202, 135, 17, 208, 105, 163, 242, 120]
    try runningSource(source, expecting: expected)
  }

  func testFirst() throws {
    let source = """
first
	"Answer the first link;  create an error if the receiver is empty."

	self emptyCheck.
	^firstLink
"""
    // 5 .. 9
    let expected = [112, 208, 135, 0, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddFirst() throws {
    let source = """
addFirst: aLink
	"Add aLink to the beginning of the receiver's list."

	self isEmpty ifTrue: [lastLink _ aLink].
	aLink nextLink: firstLink.
	firstLink _ aLink.
	^aLink
"""
    // 7 .. 19
    let expected = [112, 208, 153, 16, 97, 16, 0, 225, 135, 16, 96, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsEmpty() throws {
    let source = """
isEmpty
	^firstLink == nil
"""
    // 3 .. 6
    let expected = [0, 115, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddLast() throws {
    let source = """
addLast: aLink
	"Add aLink to the end of the receiver's list."

	self isEmpty
		ifTrue: [firstLink _ aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink _ aLink.
	^aLink
"""
    // 7 .. 21
    let expected = [112, 209, 155, 16, 129, 0, 146, 1, 16, 224, 135, 16, 97, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: aLink ifAbsent: aBlock
	"Remove aLink from the receiver.  If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink _ aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink _ nil]]
		ifFalse: [tempLink _ firstLink.
				[tempLink == nil ifTrue: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink _ tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink _ tempLink]].
	aLink nextLink: nil.
	^aLink
"""
    // 7 .. 66
    let expected = [16, 0, 198, 172, 14, 16, 208, 96, 16, 1, 198, 155, 115, 129, 1, 144, 115, 164, 34, 0, 106, 18, 115, 198, 154, 17, 201, 124, 18, 208, 16, 198, 168, 5, 18, 208, 106, 163, 238, 18, 16, 208, 225, 135, 16, 1, 198, 155, 18, 129, 1, 144, 115, 135, 16, 115, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testLast() throws {
    let source = """
last
	"Answer the last link;  create an error if the receiver is empty."

	self emptyCheck.
	^lastLink
"""
    // 5 .. 9
    let expected = [112, 208, 135, 1, 124]
    try runningSource(source, expecting: expected)
  }
}
