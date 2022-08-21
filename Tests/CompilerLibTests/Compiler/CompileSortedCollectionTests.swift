import XCTest
@testable import CompilerLib

final class CompileSortedCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("SortedCollection", instanceVariables: ["firstIndex", "lastIndex", "sortBlock"])
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

  func testIndexForInserting() throws {
    let source = """
indexForInserting: newObject
	| index low high |
	low _ firstIndex.
	high _ lastIndex.
	[index _ high + low // 2.
	low > high]
		whileFalse:
			[(sortBlock value: (self basicAt: index) value: newObject)
				ifTrue: [low _ index + 1]
				ifFalse: [high _ index - 1]].
	^low

"""
    // 7 .. 44
    let expected = [0, 106, 1, 107, 19, 18, 176, 119, 189, 105, 18, 19, 179, 168, 21, 2, 112, 17, 225, 16, 240, 157, 17, 118, 176, 129, 66, 148, 17, 118, 177, 129, 67, 135, 163, 224, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testSwapWith() throws {
    let source = """
swap: i with: j
	| t |
	t _ self basicAt: i.
	self basicAt: i put: (self basicAt: j).
	self basicAt: j put: t

"""
    // 7 .. 23
    let expected = [112, 16, 224, 106, 112, 16, 112, 17, 224, 241, 135, 112, 17, 18, 241, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyEmpty() throws {
    let source = """
copyEmpty
	"Answer a copy of the receiver without any of the receiver's elements."

	^SortedCollection sortBlock: sortBlock

"""
    // 7 .. 10
    let expected = [65, 2, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testGrow() throws {
    let source = """
grow
	"We must duplicate this message from OrderedCollection so the addLast: won't cause an error."

	| newSelf |
	newSelf _ self species new: self size + self growSize.
	newSelf sortBlock: sortBlock.
	newSelf addAll: self.
	self become: newSelf

"""
    // 13 .. 34
    let expected = [112, 208, 112, 194, 112, 209, 176, 205, 104, 16, 2, 226, 135, 16, 112, 227, 135, 112, 16, 228, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReSort() throws {
    let source = """
reSort
	self sort: firstIndex to: lastIndex

"""
    // 5 .. 10
    let expected = [112, 0, 1, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSortTo() throws {
    let source = """
sort: i to: j
	"Sort elements i through j of self to be nondescending according to sortBlock."

	| di dij dj tt ij k l n |
	"The prefix d means the data at that index."
	(n _ j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort."
	 "Sort di,dj."
	di _ self basicAt: i.
	dj _ self basicAt: j.
	(sortBlock value: di value: dj) "i.e., should di precede dj?"
		ifFalse:
			[self swap: i with: j.
			 tt _ di.
			 di _ dj.
			 dj _ tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij _ (i + j) // 2.  "ij is the midpoint of i and j."
			 dij _ self basicAt: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (sortBlock value: di value: dij) "i.e. should di precede dij?"
			   ifTrue:
				[(sortBlock value: dij value: dj) "i.e., should dij precede dj?"
				  ifFalse:
					[self swap: j with: ij.
					 dij _ dj]]
			   ifFalse:  "i.e. di should come after dij"
				[self swap: i with: ij.
				 dij _ di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k _ i.
				 l _ j.
				 [[l _ l - 1.  k <= l and: [sortBlock value: dij value: (self basicAt: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k _ k + 1.  k <= l and: [sortBlock value: (self basicAt: k) value: dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[self swap: k with: l].
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self sort: i to: l.
				self sort: k to: j]]

"""
    // 15 .. 177
    let expected = [17, 118, 176, 16, 177, 129, 73, 118, 180, 152, 120, 112, 16, 224, 106, 112, 17, 224, 108, 2, 18, 20, 242, 168, 11, 112, 16, 17, 241, 135, 18, 109, 20, 106, 21, 108, 25, 119, 179, 172, 121, 16, 17, 176, 119, 189, 110, 112, 22, 224, 107, 2, 18, 19, 242, 172, 16, 2, 19, 20, 242, 153, 115, 151, 112, 17, 22, 241, 135, 20, 129, 67, 151, 112, 16, 22, 241, 135, 18, 129, 67, 135, 25, 37, 179, 172, 75, 16, 111, 17, 130, 72, 137, 117, 200, 164, 18, 24, 118, 177, 130, 72, 23, 24, 180, 158, 2, 19, 112, 24, 224, 242, 144, 114, 125, 211, 135, 137, 117, 200, 164, 17, 23, 118, 176, 111, 23, 24, 180, 158, 2, 112, 23, 224, 19, 242, 144, 114, 125, 211, 135, 23, 24, 180, 158, 112, 23, 24, 241, 135, 163, 196, 112, 16, 24, 244, 135, 112, 23, 17, 244, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testCollect() throws {
    let source = """
collect: aBlock
	"Evaluate aBlock with each of my elements as the argument.  Collect the
	resulting values into an OrderedCollection  Answer with the new collection.
	Override superclass in order to produce OrderedCollection instead of
	SortedCollection. "

	| newCollection |
	newCollection _ OrderedCollection new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection

"""
    // 7 .. 26
    let expected = [64, 204, 105, 112, 137, 118, 200, 164, 7, 106, 17, 16, 18, 202, 225, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddAll() throws {
    let source = """
addAll: aCollection
	aCollection size > (self size // 3)
		ifTrue:
			["Faster to add the new elements and resort"
			aCollection do: [:each | super addLast: each].
			self reSort]
		ifFalse: ["Faster to add the elements individually in their proper places"
			aCollection do: [:each | self add: each]]

"""
    // 13 .. 53
    let expected = [16, 194, 112, 194, 35, 189, 179, 172, 18, 16, 137, 118, 200, 164, 6, 105, 112, 17, 133, 33, 125, 203, 135, 112, 210, 164, 12, 16, 137, 118, 200, 164, 5, 105, 112, 17, 224, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSortBlock() throws {
    let source = """
sortBlock: aBlock
	"Make the argument, aBlock, be the criterion for ordering elements of the
	receiver."

	sortBlock _ aBlock fixTemps.
	"The sortBlock must copy its home context, so as to avoid circularities!"
	"Therefore sortBlocks with side effects may not work right"
	self size > 0 ifTrue: [self reSort]

"""
    // 7 .. 18
    let expected = [16, 208, 98, 112, 194, 117, 179, 154, 112, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: newObject
	| nextIndex |
	self isEmpty ifTrue: [^super addLast: newObject].
	nextIndex _ self indexForInserting: newObject.
	self insert: newObject before: nextIndex.
	^newObject

"""
    // 13 .. 31
    let expected = [112, 209, 156, 112, 16, 133, 32, 124, 112, 16, 226, 105, 112, 16, 17, 243, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	| newCollection |
	newCollection _ self species sortBlock: sortBlock.
	newCollection addAll: self.
	^newCollection

"""
    // 9 .. 19
    let expected = [112, 209, 2, 224, 104, 16, 112, 226, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= aSortedCollection
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	self species = aSortedCollection species ifFalse: [^false].
	sortBlock = aSortedCollection sortBlock
		ifTrue: [^super = aSortedCollection]
		ifFalse: [^false]

"""
    // 11 .. 29
    let expected = [112, 208, 16, 208, 182, 168, 1, 122, 2, 16, 210, 182, 156, 112, 16, 133, 33, 124, 122]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: anInteger put: anObject
	"Storing into a SortedCollection with at:put: is not allowed."

	self error: 'to add to a sorted collection, you must use add:'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
