import XCTest
@testable import CompilerLib

final class CompileOrderedCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("OrderedCollection", instanceVariables: ["firstIndex", "lastIndex"])
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

  func testErrorFirstObject() throws {
    let source = """
errorFirstObject
	self error: 'specified object is first object'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReverse() throws {
    let source = """
reverse
	"Answer with a new collection like me with its elements in the opposite order.
	Override superclass in order to use add:, not at:put:."

	| newCollection |
	newCollection _ self species new.
	self reverseDo: [:each | newCollection add: each].
	^newCollection

"""
    // 9 .. 27
    let expected = [112, 208, 204, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testLast() throws {
    let source = """
last
	"Answer the last element.  If the receiver is empty, create an errror message.
	This is a little faster than the implementation in the superclass"

	self emptyCheck.
	^self basicAt: lastIndex

"""
    // 7 .. 13
    let expected = [112, 208, 135, 112, 1, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	^lastIndex - firstIndex + 1

"""
    // 3 .. 8
    let expected = [1, 0, 177, 118, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveAllSuchThat() throws {
    let source = """
removeAllSuchThat: aBlock
	"Evaluate aBlock for each element of the receiver.  Remove each element for
	which aBlock evaluates to true.
	A subclass might have to override this message to initialize additional instance
	variables for newCollection"

	| index element newCollection |
	newCollection _ self species new.
	index _ firstIndex.
	[index <= lastIndex]
		whileTrue:
			[element _ self basicAt: index.
			(aBlock value: element)
				ifTrue:
					[newCollection add: element.
					self removeIndex: index]
				ifFalse: [index _ index + 1]].
	^newCollection

"""
    // 11 .. 47
    let expected = [112, 208, 204, 107, 0, 105, 17, 1, 180, 172, 24, 112, 17, 225, 106, 16, 18, 202, 159, 19, 18, 226, 135, 112, 17, 227, 148, 17, 118, 176, 129, 65, 135, 163, 227, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testFirst() throws {
    let source = """
first
	"Answer the first element.  If the receiver is empty, create an errror message.
	This is a little faster than the implementation in the superclass"

	self emptyCheck.
	^self basicAt: firstIndex

"""
    // 7 .. 13
    let expected = [112, 208, 135, 112, 0, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	"override the superclass for performance"

	| index |
	index _ firstIndex.
	[index <= lastIndex]
		whileTrue:
			[aBlock value: (self basicAt: index).
			index _ index + 1]

"""
    // 5 .. 24
    let expected = [0, 105, 17, 1, 180, 172, 12, 16, 112, 17, 224, 202, 135, 17, 118, 176, 105, 163, 239, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveFirst() throws {
    let source = """
removeFirst
	"Remove the first element of the receiver.  If the receiver is empty, create an error
	message."

	| firstObject |
	self emptyCheck.
	firstObject _ self basicAt: firstIndex.
	self basicAt: firstIndex put: nil.
	firstIndex _ firstIndex + 1.
	^firstObject

"""
    // 9 .. 26
    let expected = [112, 208, 135, 112, 0, 225, 104, 112, 0, 115, 242, 135, 0, 118, 176, 96, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyWithout() throws {
    let source = """
copyWithout: oldElement
	"Answer a copy of the receiver that does not contain any elements equal
	to oldElement."

	| newCollection each |
	newCollection _ self species new: self size.
	self do: [:each | oldElement = each ifFalse: [newCollection add: each]].
	^newCollection

"""
    // 7 .. 33
    let expected = [112, 208, 112, 194, 205, 105, 112, 137, 118, 200, 164, 11, 106, 16, 18, 182, 153, 115, 146, 17, 18, 225, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testFind() throws {
    let source = """
find: oldObject
	| index |
	index _ firstIndex.
	[index <= lastIndex and: [oldObject ~= (self basicAt: index)]]
		whileTrue: [index _ index + 1].
	index <= lastIndex
		ifTrue: [^index]
		ifFalse: [self errorNotFound]

"""
    // 7 .. 36
    let expected = [0, 105, 17, 1, 180, 157, 16, 112, 17, 224, 183, 144, 114, 157, 17, 118, 176, 105, 163, 238, 17, 1, 180, 153, 17, 124, 112, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyReplaceFromToWith() throws {
    let source = """
copyReplaceFrom: start to: stop with: replacementCollection
	"Answer a copy of the receiver with replacementCollection's elements
	in place of the receiver's start'th to stop'th elements.
	This does not expect a 1-1 map from replacementCollection to the
	start to stop elements, so it will do an insert or append."

	| newOrderedCollection delta newIndex index mySize startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front.
	if start greater than self size, ignore stop and assume this is appending.
	otherwise, it is replacing part of me and start and stop have to be within my
	bounds. "
	delta _ 0.
	startIndex _ start.
	stopIndex _ stop.
	start < 1
		ifTrue: [startIndex _ stopIndex _ 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex _ stopIndex _ self size + 1]
				ifFalse:
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [self errorOutOfBounds].
					delta _ stopIndex - startIndex + 1]].
	newOrderedCollection _
		self species new: self size + replacementCollection size - delta.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do:
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection

"""
    // 11 .. 136
    let expected = [117, 108, 16, 130, 72, 17, 130, 73, 16, 118, 178, 158, 117, 129, 73, 129, 72, 164, 39, 24, 112, 194, 179, 172, 10, 112, 194, 118, 176, 129, 73, 129, 72, 164, 23, 25, 24, 118, 177, 178, 153, 113, 147, 25, 112, 194, 179, 154, 112, 208, 135, 25, 24, 177, 118, 176, 129, 68, 135, 112, 209, 112, 194, 18, 194, 176, 20, 177, 205, 107, 118, 24, 118, 177, 137, 118, 200, 164, 7, 110, 19, 112, 22, 192, 227, 125, 242, 135, 118, 18, 194, 137, 118, 200, 164, 7, 110, 19, 18, 22, 192, 227, 125, 242, 135, 25, 118, 176, 112, 194, 137, 118, 200, 164, 7, 110, 19, 112, 22, 192, 227, 125, 242, 135, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddFirst() throws {
    let source = """
addFirst: newObject
	"Add newObject to the beginning of the receiver.  Add newObject."

	firstIndex = 1 ifTrue: [self makeRoomAtFirst].
	firstIndex _ firstIndex - 1.
	self basicAt: firstIndex put: newObject.
	^newObject

"""
    // 7 .. 24
    let expected = [0, 118, 182, 154, 112, 208, 135, 0, 118, 177, 96, 112, 0, 16, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyEmpty() throws {
    let source = """
copyEmpty
	"Answer a copy of the receiver that contains no elements."

	^self species new

"""
    // 5 .. 8
    let expected = [112, 208, 204, 124]
    try runningSource(source, expecting: expected)
  }

  func testGrow() throws {
    let source = """
grow
	"Become larger.
	Typically, a subclass has to override this if the subclass adds instance variables"

	| newSelf |
	newSelf _ self species new: self size + self growSize.
	self do: [:each | newSelf addLast: each].
	self become: newSelf

"""
    // 11 .. 37
    let expected = [112, 208, 112, 194, 112, 209, 176, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 203, 135, 112, 16, 227, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCollect() throws {
    let source = """
collect: aBlock
	"Evaluate aBlock with each of my elements as the argument.  Collect the
	resulting values into a collection that is like me.  Answer with the new
	collection. Override superclass in order to use add:, not at:put:."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection

"""
    // 7 .. 27
    let expected = [112, 208, 204, 105, 112, 137, 118, 200, 164, 7, 106, 17, 16, 18, 202, 225, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: newObject
	^self addLast: newObject

"""
    // 5 .. 8
    let expected = [112, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddAll() throws {
    let source = """
addAll: anOrderedCollection
	"Add each element of anOrderedCollection at my end.  Answer anOrderedCollection."

	^self addAllLast: anOrderedCollection

"""
    // 5 .. 8
    let expected = [112, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorNotFound() throws {
    let source = """
errorNotFound
	self error: 'element not found'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: oldObject ifAbsent: absentBlock
	| index |
	index _ firstIndex.
	[index <= lastIndex]
		whileTrue:
			[oldObject = (self basicAt: index)
				ifTrue:
					[self removeIndex: index.
					^oldObject]
				ifFalse: [index _ index + 1]].
	^absentBlock value

"""
    // 7 .. 36
    let expected = [0, 106, 18, 1, 180, 172, 20, 16, 112, 18, 225, 182, 157, 112, 18, 224, 135, 16, 124, 18, 118, 176, 129, 66, 135, 163, 231, 17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: anInteger
	"Answer with my element at index anInteger.
	at: is used by a knowledgeable client to access an existing element"

	(anInteger < 1 or: [anInteger + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^super at: anInteger + firstIndex - 1]

"""
    // 9 .. 37
    let expected = [16, 118, 178, 153, 113, 150, 16, 0, 176, 118, 177, 1, 179, 155, 112, 209, 164, 9, 112, 16, 0, 176, 118, 177, 133, 32, 124, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: anInteger put: anObject
	"Put anObject at element index anInteger.
	at:put: can not be used to append, front or back, to an ordered collection;
	 it is used by a knowledgeable client to replace an element"

	| index |
	index _ anInteger truncated.
	(index < 1 or: [index + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^super at: index + firstIndex - 1 put: anObject]

"""
    // 11 .. 43
    let expected = [16, 208, 106, 18, 118, 178, 153, 113, 150, 18, 0, 176, 118, 177, 1, 179, 155, 112, 210, 164, 10, 112, 18, 0, 176, 118, 177, 17, 133, 65, 124, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyWith() throws {
    let source = """
copyWith: newElement
	"Answer a copy of the receiver that is 1 bigger than the receiver and
	includes the argument, newElement, at the end."

	| newCollection |
	newCollection _ self copy.
	newCollection add: newElement.
	^newCollection

"""
    // 7 .. 15
    let expected = [112, 208, 105, 17, 16, 225, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetIndices() throws {
    let source = """
setIndices
	firstIndex _ self basicSize // 2 max: 1.
	lastIndex _ firstIndex - 1 max: 0

"""
    // 7 .. 20
    let expected = [112, 209, 119, 189, 118, 224, 96, 0, 118, 177, 117, 224, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testReverseDo() throws {
    let source = """
reverseDo: aBlock
	"override the superclass for performance"

	| index |
	index _ lastIndex.
	[index >= firstIndex]
		whileTrue:
			[aBlock value: (self basicAt: index).
			index _ index - 1]

"""
    // 5 .. 24
    let expected = [1, 105, 17, 0, 181, 172, 12, 16, 112, 17, 224, 202, 135, 17, 118, 177, 105, 163, 239, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddLast() throws {
    let source = """
addLast: newObject
	"Add newObject to the end of the receiver.  Answer newObject."

	lastIndex = self basicSize ifTrue: [self makeRoomAtLast].
	lastIndex _ lastIndex + 1.
	self basicAt: lastIndex put: newObject.
	^newObject

"""
    // 9 .. 27
    let expected = [1, 112, 209, 182, 154, 112, 208, 135, 1, 118, 176, 97, 112, 1, 16, 242, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testInsertBefore() throws {
    let source = """
insert: anObject before: spot
	| index delta spotIndex|
	spotIndex _ spot.
	delta _ spotIndex - firstIndex.
	firstIndex = 1
		ifTrue:
			[self makeRoomAtFirst.
			spotIndex _ firstIndex + delta].
	index _ firstIndex _ firstIndex - 1.
	[index < (spotIndex - 1)]
		whileTrue:
			[self basicAt: index put: (self basicAt: index + 1).
			index _ index + 1].
	self basicAt: index put: anObject.
	^anObject

"""
    // 9 .. 60
    let expected = [17, 108, 20, 0, 177, 107, 0, 118, 182, 158, 112, 208, 135, 0, 19, 176, 108, 0, 118, 177, 129, 0, 106, 18, 20, 118, 177, 178, 172, 15, 112, 18, 112, 18, 118, 176, 226, 241, 135, 18, 118, 176, 106, 163, 234, 112, 18, 16, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorLastObject() throws {
    let source = """
errorLastObject
	self error: 'specified object is last object'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMakeRoomAtFirst() throws {
    let source = """
makeRoomAtFirst
	| delta index |
	delta _ self basicSize - self size.
	delta = 0
		ifTrue:
			[self grow.
			delta _ self basicSize - self size].
	lastIndex = self basicSize ifTrue: [^self].
	"just in case we got lucky"
	index _ self basicSize.
	[index > delta]
		whileTrue:
			[self basicAt: index put: (self basicAt: index - delta + firstIndex - 1).
			self basicAt: index - delta + firstIndex - 1 put: nil.
			index _ index - 1].
	firstIndex _ delta + 1.
	lastIndex _ self basicSize

"""
    // 11 .. 82
    let expected = [112, 208, 112, 194, 177, 104, 16, 117, 182, 172, 9, 112, 209, 135, 112, 208, 112, 194, 177, 104, 1, 112, 208, 182, 152, 120, 112, 208, 105, 17, 16, 179, 172, 30, 112, 17, 112, 17, 16, 177, 0, 176, 118, 177, 227, 242, 135, 112, 17, 16, 177, 0, 176, 118, 177, 115, 242, 135, 17, 118, 177, 105, 163, 221, 16, 118, 176, 96, 112, 208, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyFromTo() throws {
    let source = """
copyFrom: startIndex to: endIndex
	"Answer a copy of the receiver that contains elements from position startIndex
	to endIndex."

	| targetCollection index |
	endIndex < startIndex ifTrue: [^self species new: 0].
	targetCollection _ self species new: endIndex + 1 - startIndex.
	startIndex to: endIndex do: [:index | targetCollection add: (self at: index)].
	^targetCollection

"""
    // 9 .. 44
    let expected = [17, 16, 178, 156, 112, 208, 117, 205, 124, 112, 208, 17, 118, 176, 16, 177, 205, 106, 16, 17, 137, 118, 200, 164, 7, 107, 18, 112, 19, 192, 226, 125, 241, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddBefore() throws {
    let source = """
add: newObject before: oldObject
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just preceding oldObject.  Answer newObject."

	| index |
	index _ self find: oldObject.
	self insert: newObject before: index.
	^newObject

"""
    // 7 .. 17
    let expected = [112, 17, 224, 106, 112, 16, 18, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testSelect() throws {
    let source = """
select: aBlock
	"Evaluate aBlock with each of my elements as the argument.  Collect into a new
	collection like me, only those elements for which aBlock evaluates to true.
	Override superclass in order to use add:, not at:put:."

	| newCollection |
	newCollection _ self copyEmpty.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection

"""
    // 7 .. 30
    let expected = [112, 208, 105, 112, 137, 118, 200, 164, 11, 106, 16, 18, 202, 155, 17, 18, 225, 144, 115, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIndex() throws {
    let source = """
removeIndex: removedIndex
	| index |
	index _ removedIndex.
	[index < lastIndex]
		whileTrue:
			[self basicAt: index put: (self basicAt: index + 1).
			index _ index + 1].
	self basicAt: lastIndex put: nil.
	lastIndex _ lastIndex - 1

"""
    // 7 .. 38
    let expected = [16, 105, 17, 1, 178, 172, 15, 112, 17, 112, 17, 118, 176, 225, 240, 135, 17, 118, 176, 105, 163, 236, 112, 1, 115, 240, 135, 1, 118, 177, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testBefore() throws {
    let source = """
before: oldObject
	"Answer the element before oldObject.  If the receiver does not contain oldObject
	or if the receiver contains no elements before oldObject, create an error message."

	| index |
	index _ self find: oldObject.
	index = firstIndex
		ifTrue: [^self errorFirstObject]
		ifFalse: [^self basicAt: index - 1]

"""
    // 9 .. 25
    let expected = [112, 16, 224, 105, 17, 0, 182, 154, 112, 210, 124, 112, 17, 118, 177, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorNoSuchElement() throws {
    let source = """
errorNoSuchElement
	self error: 'attempt to index non-existent element in an ordered collection'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAfter() throws {
    let source = """
after: oldObject
	"Answer the element after oldObject.  If the receiver does not contain oldObject
	or if the receiver contains no elements after oldObject, create an error message."

	| index |
	index _ self find: oldObject.
	index = lastIndex
		ifTrue: [^self errorLastObject]
		ifFalse: [^self basicAt: index + 1]

"""
    // 9 .. 25
    let expected = [112, 16, 224, 105, 17, 1, 182, 154, 112, 210, 124, 112, 17, 118, 176, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testMakeRoomAtLast() throws {
    let source = """
makeRoomAtLast
	| index newLast |
	newLast _ self size.
	self basicSize - self size = 0 ifTrue: [self grow].
	firstIndex = 1 ifTrue: [^self].
	"we might be here under false premises or grow did the job for us"
	index _ 1.
	[index <= newLast]
		whileTrue:
			[self basicAt: index put: (self basicAt: index + firstIndex - 1).
			self basicAt: index + firstIndex - 1 put: nil.
			index _ index + 1].
	firstIndex _ 1.
	lastIndex _ newLast

"""
    // 11 .. 67
    let expected = [112, 194, 105, 112, 209, 112, 194, 177, 117, 182, 154, 112, 208, 135, 0, 118, 182, 152, 120, 118, 104, 16, 17, 180, 172, 26, 112, 16, 112, 16, 0, 176, 118, 177, 227, 242, 135, 112, 16, 0, 176, 118, 177, 115, 242, 135, 16, 118, 176, 104, 163, 225, 118, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddAllFirst() throws {
    let source = """
addAllFirst: anOrderedCollection
	"Add each element of anOrderedCollection at the beginning of the receiver.
	Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection

"""
    // 7 .. 21
    let expected = [16, 137, 118, 200, 164, 5, 105, 112, 17, 225, 125, 224, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveLast() throws {
    let source = """
removeLast
	"Remove the last element of the receiver.  If the receiver is empty, create an error
	message."

	| lastObject |
	self emptyCheck.
	lastObject _ self basicAt: lastIndex.
	self basicAt: lastIndex put: nil.
	lastIndex _ lastIndex - 1.
	^lastObject

"""
    // 9 .. 26
    let expected = [112, 208, 135, 112, 1, 225, 104, 112, 1, 115, 242, 135, 1, 118, 177, 97, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddAfter() throws {
    let source = """
add: newObject after: oldObject
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just succeeding oldObject.  Answer newObject."

	| index |
	index _ self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject

"""
    // 7 .. 19
    let expected = [112, 17, 224, 106, 112, 16, 18, 118, 176, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddAllLast() throws {
    let source = """
addAllLast: anOrderedCollection
	"Add each element of anOrderedCollection at the end of the receiver.  Answer
	anOrderedCollection."

	anOrderedCollection do: [:each | self addLast: each].
	^anOrderedCollection

"""
    // 5 .. 19
    let expected = [16, 137, 118, 200, 164, 5, 105, 112, 17, 224, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

}
