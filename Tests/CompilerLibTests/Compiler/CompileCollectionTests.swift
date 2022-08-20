import XCTest
@testable import CompilerLib

final class CompileCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Collection", instanceVariables: [])
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

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self do:
		[:each |
		noneYet
			ifTrue: [noneYet _ false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)

"""
    // 21 .. 75
    let expected = [16, 33, 224, 135, 16, 112, 199, 210, 224, 135, 16, 35, 224, 135, 113, 105, 112, 137, 118, 200, 164, 19, 106, 17, 155, 114, 129, 65, 146, 16, 36, 196, 135, 16, 37, 224, 135, 16, 18, 230, 125, 203, 135, 17, 168, 4, 16, 39, 224, 135, 16, 40, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCollect() throws {
    let source = """
collect: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Collect the
	resulting values into a collection that is like the receiver.  Answer the new
	collection. "

	| newCollection |
	newCollection _ self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection

"""
    // 7 .. 27
    let expected = [112, 208, 204, 105, 112, 137, 118, 200, 164, 7, 106, 17, 16, 18, 202, 225, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorNotKeyed() throws {
    let source = """
errorNotKeyed
	self error: self class name, 's do not respond to keyed accessing messages.'

"""
    // 11 .. 19
    let expected = [112, 112, 199, 210, 35, 225, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMaxPrint() throws {
    let source = """
maxPrint
	"Answer the maximum number of characters to print with printOn:."
	^5000

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testGrowSize() throws {
    let source = """
growSize
	"Answer an amount by which the receiver should grow to make room for more elements (in response to the message 'grow')."

	self basicSize >= self maxSize ifTrue: [self error: 'unable to grow this collection'].
	^(self basicSize max: 2) min: self maxSize - self basicSize

"""
    // 15 .. 35
    let expected = [112, 210, 112, 211, 181, 155, 112, 33, 224, 135, 112, 210, 119, 229, 112, 211, 112, 210, 177, 228, 124]
    try runningSource(source, expecting: expected)
  }

  func testMaxSize() throws {
    let source = """
maxSize
	"Answer the largest basicSize which is valid for the receiver's class."

	^65486 "for VM3 interpreter DoradoST80Aug19"

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddAll() throws {
    let source = """
addAll: aCollection
	"Include all the elements of aCollection as the receiver's elements.  Answer
	aCollection."

	aCollection do: [:each | self add: each].
	^aCollection

"""
    // 5 .. 19
    let expected = [16, 137, 118, 200, 164, 5, 105, 112, 17, 224, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorNotFound() throws {
    let source = """
errorNotFound
	self error: 'Object is not in the collection.'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOccurrencesOf() throws {
    let source = """
occurrencesOf: anObject
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally _ 0.
	self do: [:each | anObject = each ifTrue: [tally _ tally + 1]].
	^tally

"""
    // 3 .. 27
    let expected = [117, 105, 112, 137, 118, 200, 164, 13, 106, 16, 18, 182, 157, 17, 118, 176, 129, 65, 144, 115, 125, 203, 135, 17, 124]
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

  func testPrintOn() throws {
    let source = """
printOn: aStream
	| tooMany |
	tooMany _ aStream position + self maxPrint.
	aStream nextPutAll: self class name, ' ('.
	self do:
		[:element |
		aStream position > tooMany ifTrue: [aStream nextPutAll: '...etc...)'. ^self].
		element printOn: aStream.
		aStream space].
	aStream nextPut: $)

"""
    // 23 .. 67
    let expected = [16, 208, 112, 209, 176, 105, 16, 112, 199, 212, 37, 227, 226, 135, 112, 137, 118, 200, 164, 18, 106, 16, 208, 17, 179, 156, 16, 38, 226, 135, 120, 18, 16, 231, 135, 16, 216, 125, 203, 135, 16, 41, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: oldObject ifAbsent: anExceptionBlock
	"Remove oldObject as one of the receiver's elements.  If several of the
	elements are equal to oldObject, only one is removed. If no element is equal to
	oldObject, answer the result of evaluating anExceptionBlock.  Otherwise,
	answer the argument, oldObject.

	SequenceableCollections can not respond to this message."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIncludes() throws {
    let source = """
includes: anObject
	"Answer whether anObject is one of the receiver's elements."

	self do: [:each | anObject = each ifTrue: [^true]].
	^false

"""
    // 3 .. 19
    let expected = [112, 137, 118, 200, 164, 8, 105, 16, 17, 182, 152, 121, 115, 125, 203, 135, 122]
    try runningSource(source, expecting: expected)
  }

  func testReject() throws {
    let source = """
reject: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to false.  Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]

"""
    // 5 .. 19
    let expected = [112, 137, 118, 200, 164, 7, 105, 16, 17, 202, 114, 198, 125, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveAll() throws {
    let source = """
removeAll: aCollection
	"Remove each element of aCollection from the receiver.  If successful for each,
	answer aCollection."

	aCollection do: [:each | self remove: each].
	^aCollection

"""
    // 5 .. 19
    let expected = [16, 137, 118, 200, 164, 5, 105, 112, 17, 224, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorEmptyCollection() throws {
    let source = """
errorEmptyCollection
	self error: 'this collection is empty'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsSortedCollection() throws {
    let source = """
asSortedCollection
	"Answer a new instance of SortedCollection whose elements are the elements of
	the receiver.  The sort order is the default less than or equal ordering."

	| aSortedCollection |
	aSortedCollection _ SortedCollection new: self size.
	aSortedCollection addAll: self.
	^aSortedCollection

"""
    // 7 .. 17
    let expected = [64, 112, 194, 205, 104, 16, 112, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemove() throws {
    let source = """
remove: oldObject
	"Remove oldObject as one of the receiver's elements.  Answer oldObject unless
	no element is equal to oldObject, in which case, create an error message."

	^self remove: oldObject ifAbsent: [self errorNotFound]

"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testInjectInto() throws {
    let source = """
inject: thisValue into: binaryBlock
	"Accumulate a running value associated with evaluating the argument,
	binaryBlock, with the current value and the receiver as block arguments.
	The initial value is the value of the argument, thisValue.
		For instance, to sum a collection, use:
			collection inject: 0 into: [:subTotal :next | subTotal + next]."

	| nextValue |
	nextValue _ thisValue.
	self do: [:each | nextValue _ binaryBlock value: nextValue value: each].
	^nextValue

"""
    // 5 .. 24
    let expected = [16, 106, 112, 137, 118, 200, 164, 8, 107, 17, 18, 19, 240, 129, 66, 125, 203, 135, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsOrderedCollection() throws {
    let source = """
asOrderedCollection
	"Answer a new instance of OrderedCollection whose elements are the elements of
	the receiver.  The order in which elements are added depends on the order in
	which the receiver enumerates its elements.  In the case of unordered collections,
	the ordering is not necessarily the same for multiple requests for the conversion."

	| anOrderedCollection |
	anOrderedCollection _ OrderedCollection new: self size.
	self do: [:each | anOrderedCollection addLast: each].
	^anOrderedCollection

"""
    // 7 .. 26
    let expected = [64, 112, 194, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 225, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorNoMatch() throws {
    let source = """
errorNoMatch
	self error: 'collection sizes do not match'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsSortedCollectionWith() throws {
    let source = """
asSortedCollection: aBlock
	"Answer a new instance of SortedCollection whose elements are the elements of
	the receiver.  The sort order is defined by the argument, aBlock."

	| aSortedCollection |
	aSortedCollection _ SortedCollection new: self size.
	aSortedCollection sortBlock: aBlock.
	aSortedCollection addAll: self.
	^aSortedCollection

"""
    // 9 .. 23
    let expected = [64, 112, 194, 205, 105, 17, 16, 225, 135, 17, 112, 226, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsBag() throws {
    let source = """
asBag
	"Answer a new instance of Bag whose elements are the elements of
	the receiver."

	| aBag |
	aBag _ Bag new.
	self do: [:each | aBag add: each].
	^aBag

"""
    // 7 .. 24
    let expected = [64, 204, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 225, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testDetectIfNone() throws {
    let source = """
detect: aBlock ifNone: exceptionBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value

"""
    // 3 .. 22
    let expected = [112, 137, 118, 200, 164, 9, 106, 16, 18, 202, 153, 18, 124, 115, 125, 203, 135, 17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testEmptyCheck() throws {
    let source = """
emptyCheck
	self isEmpty ifTrue: [self errorEmptyCollection]

"""
    // 7 .. 13
    let expected = [112, 209, 154, 112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsSet() throws {
    let source = """
asSet
	"Answer a new instance of Set whose elements are the unique elements of
	the receiver."

	| aSet |
	aSet _ Set new: self size.
	self do: [:each | aSet add: each].
	^aSet

"""
    // 7 .. 26
    let expected = [64, 112, 194, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 225, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testSelect() throws {
    let source = """
select: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to true.  Answer the new collection."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection

"""
    // 7 .. 31
    let expected = [112, 208, 204, 105, 112, 137, 118, 200, 164, 11, 106, 16, 18, 202, 155, 17, 18, 225, 144, 115, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testDetect() throws {
    let source = """
detect: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	^self detect: aBlock ifNone: [self errorNotFound]

"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsEmpty() throws {
    let source = """
isEmpty
	"Answer whether the receiver contains any elements."

	^self size = 0

"""
    // 3 .. 7
    let expected = [112, 194, 117, 182, 124]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: newObject
	"Include newObject as one of the receiver's elements.  Answer newObject.
	This message should not be sent to instances of subclasses of ArrayedCollection."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
