import XCTest
@testable import CompilerLib

final class CompileSequenceableCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("SequenceableCollection", instanceVariables: [])
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

  func testIndexOfIfAbsent() throws {
    let source = """
indexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer the result of evaluating the exceptionBlock."

	(1 to: self size)
		do: [:i | (self at: i) = anElement ifTrue: [^i]].
	^exceptionBlock value

"""
    // 5 .. 29
    let expected = [118, 112, 194, 224, 137, 118, 200, 164, 11, 106, 112, 18, 192, 16, 182, 153, 18, 124, 115, 125, 203, 135, 17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testLast() throws {
    let source = """
last
	"Answer the last element of the receiver.  Create an error if the receiver
	contains no elements."

	self emptyCheck.
	^self at: self size

"""
    // 5 .. 12
    let expected = [112, 208, 135, 112, 112, 194, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testShallowCopy() throws {
    let source = """
shallowCopy
	^self copyFrom: 1 to: self size

"""
    // 5 .. 10
    let expected = [112, 118, 112, 194, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testWithDo() throws {
    let source = """
with: aSequenceableCollection do: aBlock
	"Evaluate aBlock with each of the receiver's elements along with the corresponding
	element from aSequencableCollection."

	| otherCollection |
	self size ~= aSequenceableCollection size ifTrue: [^self errorNoMatch].
	otherCollection _ ReadStream on: aSequenceableCollection.
	self do: [:each | aBlock value: each value: otherCollection next]

"""
    // 11 .. 39
    let expected = [112, 194, 16, 194, 183, 154, 112, 208, 124, 66, 16, 225, 106, 112, 137, 118, 200, 164, 7, 107, 17, 19, 18, 195, 243, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReverseDo() throws {
    let source = """
reverseDo: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument, starting
	with the last element and taking each in sequence up to the first.  For
	SequenceableCollections, this is the reverse of the enumeration in do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]

"""
    // 5 .. 24
    let expected = [112, 194, 118, 116, 137, 118, 200, 164, 7, 105, 16, 112, 17, 192, 202, 125, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCollect() throws {
    let source = """
collect: aBlock
	| aStream index length |
	aStream _ WriteStream on: (self species new: self size).
	index _ 0.
	length _ self size.
	[(index _ index + 1) <= length]
		whileTrue: [aStream nextPut: (aBlock value: (self at: index))].
	^aStream contents

"""
    // 11 .. 45
    let expected = [65, 112, 210, 112, 194, 205, 224, 105, 117, 106, 112, 194, 107, 18, 118, 176, 129, 66, 19, 180, 172, 10, 17, 16, 112, 18, 192, 202, 196, 135, 163, 237, 17, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testFirst() throws {
    let source = """
first
	"Answer the first element of the receiver.  Create an error if the receiver
	contains no elements."

	self emptyCheck.
	^self at: 1

"""
    // 5 .. 11
    let expected = [112, 208, 135, 112, 118, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testIndexOf() throws {
    let source = """
indexOf: anElement
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]

"""
    // 5 .. 15
    let expected = [112, 16, 137, 117, 200, 164, 2, 117, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testReplaceFromToWithStartingAt() throws {
    let source = """
replaceFrom: start to: stop with: replacement startingAt: repStart
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."

	| index repOff |
	repOff _ repStart - start.
	index _ start - 1.
	[(index _ index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]

"""
    // 3 .. 31
    let expected = [19, 16, 177, 109, 16, 118, 177, 108, 20, 118, 176, 129, 68, 17, 180, 172, 11, 112, 20, 18, 21, 20, 176, 192, 193, 135, 163, 236, 120]
    try runningSource(source, expecting: expected)
  }

  func testReverse() throws {
    let source = """
reverse
	"Answer with a new collection like me with its elements in the opposite order."

	| aStream index length |
	aStream _ WriteStream on: (self species new: self size).
	index _ self size + 1.
	[(index _ index - 1) > 0]
		whileTrue: [aStream nextPut: (self at: index)].
	^aStream contents

"""
    // 11 .. 42
    let expected = [65, 112, 210, 112, 194, 205, 224, 104, 112, 194, 118, 176, 105, 17, 118, 177, 129, 65, 117, 179, 159, 16, 112, 17, 192, 196, 135, 163, 240, 16, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyWithout() throws {
    let source = """
copyWithout: oldElement
	"Answer a copy of the receiver in which all occurrences of oldElement
	have been left out."

	| aStream |
	aStream _ WriteStream on: (self species new: self size).
	self do: [:each | oldElement = each ifFalse: [aStream nextPut: each]].
	^aStream contents

"""
    // 11 .. 40
    let expected = [65, 112, 210, 112, 194, 205, 224, 105, 112, 137, 118, 200, 164, 11, 106, 16, 18, 182, 153, 115, 146, 17, 18, 196, 125, 203, 135, 17, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: oldObject ifAbsent: anExceptionBlock
	"SequencableCollections cannot implement removing."

	self shouldNotImplement

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSwapWith() throws {
    let source = """
swap: oneIndex with: anotherIndex
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element _ self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element

"""
    // 3 .. 19
    let expected = [112, 16, 192, 106, 112, 16, 112, 17, 192, 193, 135, 112, 17, 18, 193, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testGrow() throws {
    let source = """
grow
	"The receiver becomes bigger--this is not a copy of the receiver, so all shared references survive."

	| newArray |
	newArray _ self species new: self size + self growSize.
	newArray replaceFrom: 1 to: self size with: self.
	^self become: newArray

"""
    // 11 .. 31
    let expected = [112, 208, 112, 194, 112, 209, 176, 205, 104, 16, 118, 112, 194, 112, 131, 98, 135, 112, 16, 227, 124]
    try runningSource(source, expecting: expected)
  }

  func testFindLast() throws {
    let source = """
findLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index _ self size + 1.
	[(index _ index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0

"""
    // 3 .. 28
    let expected = [112, 194, 118, 176, 105, 17, 118, 177, 129, 65, 118, 181, 172, 10, 16, 112, 17, 192, 202, 153, 17, 124, 163, 237, 117, 124]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtAllPut() throws {
    let source = """
atAllPut: anObject
	"Put anObject at every one of the receiver's indices."

	| index size |
	index _ 0.
	size _ self size.
	[(index _ index + 1) <= size]
		whileTrue: [self at: index put: anObject]

"""
    // 3 .. 23
    let expected = [117, 105, 112, 194, 106, 17, 118, 176, 129, 65, 18, 180, 158, 112, 17, 16, 193, 135, 163, 241, 120]
    try runningSource(source, expecting: expected)
  }

  func testSelect() throws {
    let source = """
select: aBlock
	| aStream index length |
	aStream _ WriteStream on: (self species new: self size).
	index _ 0.
	length _ self size.
	[(index _ index + 1) <= length]
		whileTrue:
			[(aBlock value: (self at: index)) ifTrue: [aStream nextPut: (self at: index)]].
	^aStream contents

"""
    // 11 .. 49
    let expected = [65, 112, 210, 112, 194, 205, 224, 105, 117, 106, 112, 194, 107, 18, 118, 176, 129, 66, 19, 180, 172, 14, 16, 112, 18, 192, 202, 157, 17, 112, 18, 192, 196, 135, 163, 233, 17, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyReplaceFromToWith() throws {
    let source = """
copyReplaceFrom: start to: stop with: replacementCollection
	"Answer a copy of the receiver satisfying the following conditions:

	If stop is less than start, then this is an insertion;
		stop should be exactly start-1,
		start = 1 means insert before the first character,
		start = size+1 means append after last character.
	Otherwise, this is a replacement;
		start and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize _ self size - (stop - start + 1) + replacementCollection size.
	endReplacement _ start - 1 + replacementCollection size.
	newSequenceableCollection _ self species new: newSize.
	newSequenceableCollection
		replaceFrom: 1
		to: start - 1
		with: self
		startingAt: 1.
	newSequenceableCollection
		replaceFrom: start
		to: endReplacement
		with: replacementCollection
		startingAt: 1.
	newSequenceableCollection
		replaceFrom: endReplacement + 1
		to: newSize
		with: self
		startingAt: stop + 1.
	^newSequenceableCollection

"""
    // 7 .. 62
    let expected = [112, 194, 17, 16, 177, 118, 176, 177, 18, 194, 176, 108, 16, 118, 177, 18, 194, 176, 109, 112, 208, 20, 205, 107, 19, 118, 16, 118, 177, 112, 118, 131, 129, 135, 19, 16, 21, 18, 118, 131, 129, 135, 19, 21, 118, 176, 20, 112, 17, 118, 176, 131, 129, 135, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testComma() throws {
    let source = """
, aSequenceableCollection
	"Answer with a copy of the receiver concatenated with the argument,
	a SequencableCollection."

	^self copyReplaceFrom: self size + 1
		  to: self size
		  with: aSequenceableCollection

"""
    // 5 .. 15
    let expected = [112, 112, 194, 118, 176, 112, 194, 16, 131, 96, 124]
    try runningSource(source, expecting: expected)
  }

  func testIndexOfSubCollectionStartingAt() throws {
    let source = """
indexOfSubCollection: aSubCollection startingAt: anIndex
	"Answer the index of the receiver's first element, such that that element
	equals the first element of aSubCollection, and the next elements equal the rest of
	the elements of aSubCollection.  Begin the search at element anIndex of the
	receiver.  If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]

"""
    // 5 .. 17
    let expected = [112, 16, 17, 137, 117, 200, 164, 2, 117, 125, 131, 96, 124]
    try runningSource(source, expecting: expected)
  }

  func testIndexOfSubCollectionStartingAtIfAbsent() throws {
    let source = """
indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: exceptionBlock
	"Answer the index of the receiver's first element, such that that element
	equals the first element of aSubCollection, and the next elements equal the rest of
	the elements of aSubCollection.  Begin the search at element anIndex of the
	receiver.  If no such match is found, answer the result of evaluating exceptionBlock."

	| firstElement eq |
	aSubCollection size = 0 ifTrue: [^exceptionBlock value].
	firstElement _ aSubCollection at: 1.
	anIndex to: self size - aSubCollection size + 1 do:
		[:i |
		eq _ true.
		(self at: i) = firstElement
			ifTrue:
				[1 to: aSubCollection size do:
					[:index |
					(self at: i + index - 1) = (aSubCollection at: index)
						ifFalse: [eq _ false]].
				eq ifTrue: [^i]]].
	^exceptionBlock value

"""
    // 5 .. 81
    let expected = [16, 194, 117, 182, 154, 18, 201, 124, 16, 118, 192, 107, 17, 112, 194, 16, 194, 177, 118, 176, 137, 118, 200, 164, 47, 109, 113, 108, 112, 21, 192, 19, 182, 172, 35, 118, 16, 194, 137, 118, 200, 164, 19, 110, 112, 21, 22, 176, 118, 177, 192, 16, 22, 192, 182, 153, 115, 146, 114, 129, 68, 125, 240, 135, 20, 153, 21, 124, 115, 144, 115, 125, 240, 135, 18, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= otherCollection
	"Answer whether the species of the receiver is the same as otherCollection's species,
	and the receiver's size is the same as otherCollection's size, and each of the receiver's
	elements equal the corresponding element of otherCollection"

	| size index |
	(size _ self size) = otherCollection size ifFalse: [^false].
	self species == otherCollection species ifFalse: [^false].
	index _ 0.
	[(index _ index + 1) <= size]
		whileTrue: [(self at: index) = (otherCollection at: index) ifFalse: [^false]].
	^true

"""
    // 5 .. 46
    let expected = [112, 194, 129, 65, 16, 194, 182, 168, 1, 122, 112, 208, 16, 208, 198, 168, 1, 122, 117, 106, 18, 118, 176, 129, 66, 17, 180, 172, 12, 112, 18, 192, 16, 18, 192, 182, 168, 1, 122, 163, 235, 121]
    try runningSource(source, expecting: expected)
  }

  func testMappedBy() throws {
    let source = """
mappedBy: aSequenceableCollection
	"Answer a new instance of MappedCollection whose contents is the
	receiver and whose map is the argument, aSequencableCollection."

	^(MappedCollection collection: self map: aSequenceableCollection) contents

"""
    // 9 .. 14
    let expected = [66, 112, 16, 241, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorOutOfBounds() throws {
    let source = """
errorOutOfBounds
	self error: 'indices are out of bounds'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtAllNPut() throws {
    let source = """
atAll: anInterval put: anObject
	"Put anObject at every index specified by the integer elements of anInterval."

	anInterval do: [:index | self at: index put: anObject]

"""
    // 3 .. 17
    let expected = [16, 137, 118, 200, 164, 6, 106, 112, 18, 17, 193, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsArray() throws {
    let source = """
asArray
	"Answer a new instance of Array whose elements are the elements of
	the receiver, in the same order."

	| newArray |
	newArray _ Array new: self size.
	1 to: self size do: [:index | newArray at: index put: (self at: index)].
	^newArray

"""
    // 7 .. 31
    let expected = [64, 112, 194, 205, 104, 118, 112, 194, 137, 118, 200, 164, 8, 105, 16, 17, 112, 17, 192, 193, 125, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	| index length |
	index _ 0.
	length _ self size.
	[(index _ index + 1) <= length]
		whileTrue: [aBlock value: (self at: index)]

"""
    // 3 .. 24
    let expected = [117, 105, 112, 194, 106, 17, 118, 176, 129, 65, 18, 180, 159, 16, 112, 17, 192, 202, 135, 163, 240, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyWith() throws {
    let source = """
copyWith: newElement
	"Answer a copy of the receiver that is 1 bigger than the receiver and has
	newElement at the last element."

	| newIC |
	newIC _ self species new: self size + 1.
	newIC
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1.
	newIC at: newIC size put: newElement.
	^newIC

"""
    // 7 .. 31
    let expected = [112, 208, 112, 194, 118, 176, 205, 105, 17, 118, 112, 194, 112, 118, 131, 129, 135, 17, 17, 194, 16, 193, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyReplaceAllWith() throws {
    let source = """
copyReplaceAll: oldSubstring with: newSubstring
	"Answer a copy of the receiver in which all occurrances of oldSubstring have
	been replaced by newSubstring."

	| aString startSearch currentIndex |
	aString _ self.
	startSearch _ 1.
	[(currentIndex _ aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue:
			[aString _ aString
						copyReplaceFrom: currentIndex
						to: currentIndex + oldSubstring size - 1
						with: newSubstring.
			startSearch _ currentIndex + newSubstring size].
	^aString
	"'How noww brown cowow?' copyReplaceAll: 'ow' with: 'ello'"

"""
    // 7 .. 41
    let expected = [112, 106, 118, 107, 18, 16, 19, 241, 129, 68, 117, 179, 172, 19, 18, 20, 20, 16, 194, 176, 118, 177, 17, 131, 96, 106, 20, 17, 194, 176, 107, 163, 227, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testReplaceFromToWith() throws {
    let source = """
replaceFrom: start to: stop with: replacement
	"This destructively replaces elements from start to stop in the receiver.
	Answer the receiver itself.
	Use copyReplaceFrom:to:with: for insertion/deletion which may alter the
	size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1

"""
    // 9 .. 30
    let expected = [18, 194, 17, 16, 177, 118, 176, 182, 168, 4, 112, 33, 224, 135, 112, 16, 17, 18, 118, 131, 130, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyFromTo() throws {
    let source = """
copyFrom: start to: stop
	"Answer a copy of a subset of the receiver, starting from element at index start
	until element at index stop."

	| newSize |
	newSize _ stop - start + 1.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start

"""
    // 7 .. 23
    let expected = [17, 16, 177, 118, 176, 106, 112, 209, 18, 205, 118, 18, 112, 16, 131, 128, 124]
    try runningSource(source, expecting: expected)
  }

  func testFindFirst() throws {
    let source = """
findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	| index |
	index _ 0.
	[(index _ index + 1) <= self size] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0

"""
    // 3 .. 26
    let expected = [117, 105, 17, 118, 176, 129, 65, 112, 194, 180, 172, 10, 16, 112, 17, 192, 202, 153, 17, 124, 163, 236, 117, 124]
    try runningSource(source, expecting: expected)
  }

}
