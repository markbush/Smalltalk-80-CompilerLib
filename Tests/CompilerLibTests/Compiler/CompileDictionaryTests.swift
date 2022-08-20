import XCTest
@testable import CompilerLib

final class CompileDictionaryTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Dictionary", instanceVariables: ["tally"])
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

  func testRemoveIfAbsent() throws {
    let source = """
remove: anObject ifAbsent: exceptionBlock
	self shouldNotImplement
"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAsSortedCollection() throws {
    let source = """
asSortedCollection
	| aSortedCollection |
	aSortedCollection _ SortedCollection new: self size.
	self associationsDo: [:association | aSortedCollection add: association].
	^aSortedCollection
"""
    // 9 .. 28
    let expected = [64, 112, 194, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemove() throws {
    let source = """
remove: anObject
	self shouldNotImplement
"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testGrow() throws {
    let source = """
grow
	| newSelf |
	newSelf _ self species new: self basicSize + self growSize.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	self become: newSelf
"""
    // 15 .. 41
    let expected = [112, 208, 112, 209, 112, 210, 176, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 228, 125, 227, 135, 112, 16, 229, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: anAssociation
	| index element |
	index _ self findKeyOrNil: anAssociation key.
	element _ self basicAt: index.
	element == nil
		ifTrue: [self atNewIndex: index put: anAssociation]
		ifFalse: [element value: anAssociation value].
	^anAssociation
"""
    // 11 .. 35
    let expected = [112, 16, 209, 224, 105, 112, 17, 226, 106, 18, 115, 198, 156, 112, 17, 16, 243, 147, 18, 16, 201, 202, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self associationsDo:
			[:each |
			noneYet
				ifTrue: [noneYet _ false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
"""
    // 23 .. 77
    let expected = [16, 33, 224, 135, 16, 112, 199, 210, 224, 135, 16, 35, 224, 135, 113, 105, 112, 137, 118, 200, 164, 19, 106, 17, 155, 114, 129, 65, 146, 16, 37, 196, 135, 16, 38, 224, 135, 16, 18, 231, 125, 228, 135, 17, 168, 4, 16, 40, 224, 135, 16, 41, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSelect() throws {
    let source = """
select: aBlock
	"Evaluate aBlock with each of my values as the argument.  Collect into a new
	dictionary, only those associations for which aBlock evaluates to true."

	| newCollection |
	newCollection _ self species new.
	self associationsDo:
		[:each |
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection
"""
    // 9 .. 34
    let expected = [112, 208, 204, 105, 112, 137, 118, 200, 164, 12, 106, 16, 18, 201, 202, 155, 17, 18, 226, 144, 115, 125, 225, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	| tooMany |
	tooMany _ aStream position + self maxPrint.
	aStream nextPutAll: self class name, ' ('.
	self associationsDo:
		[:element |
		aStream position > tooMany ifTrue: [aStream nextPutAll: '...etc...)'. ^self].
		element printOn: aStream.
		aStream space].
	aStream nextPut: $)
"""
    // 25 .. 69
    let expected = [16, 208, 112, 209, 176, 105, 16, 112, 199, 212, 37, 227, 226, 135, 112, 137, 118, 200, 164, 18, 106, 16, 208, 17, 179, 156, 16, 39, 226, 135, 120, 18, 16, 232, 135, 16, 217, 125, 230, 135, 16, 42, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testInspect() throws {
    let source = """
inspect
	"Create and schedule a DictionaryInspector in which the user can examine the
	receiver's variables."

	InspectorView open: (DictionaryInspector inspect: self)
"""
    // 11 .. 17
    let expected = [65, 67, 112, 226, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	super do: [:assoc | aBlock value: assoc value]
"""
    // 7 .. 22
    let expected = [112, 137, 118, 200, 164, 6, 105, 16, 17, 201, 202, 125, 133, 32, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFindKeyOrNil() throws {
    // backslash doubled in quoted string!
    let source = """
findKeyOrNil: key
	| location length probe pass |
	length _ self basicSize.
	pass _ 1.
	location _ key hash \\\\ length + 1.
	[(probe _ self basicAt: location) == nil or: [probe key = key]]
		whileFalse:
			[(location _ location + 1) > length
				ifTrue:
					[location _ 1.
					pass _ pass + 1.
					pass > 2 ifTrue: [^self grow findKeyOrNil: key]]].
	^location
"""
    // 15 .. 70
    let expected = [112, 208, 106, 118, 108, 16, 209, 18, 186, 118, 176, 105, 112, 17, 229, 129, 67, 115, 198, 153, 113, 147, 19, 212, 16, 182, 168, 26, 17, 118, 176, 129, 65, 18, 179, 172, 15, 118, 105, 20, 118, 176, 108, 20, 119, 179, 156, 112, 211, 16, 226, 124, 163, 214, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testDeclareFrom() throws {
    let source = """
declare: key from: aDictionary
	"Add key to the receiver.  If key already exists, do nothing.  If aDictionary includes
	key, then remove it from aDictionary and use its association as the entry to the
	receiver."

	(self includesKey: key) ifTrue: [^self].
	(aDictionary includesKey: key)
		ifTrue:
			[self add: (aDictionary associationAt: key).
			aDictionary removeKey: key]
		ifFalse:
			[self at: key put: nil]
"""
    // 11 .. 36
    let expected = [112, 16, 224, 152, 120, 17, 16, 224, 172, 10, 112, 17, 16, 226, 225, 135, 17, 16, 227, 147, 112, 16, 115, 193, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIncludesAssociation() throws {
    let source = """
includesAssociation: anAssociation
	"Answer whether the receiver has an element (association between a key and
	a value) that is equal to the argument, anAssociation."

	^super includes: anAssociation
"""
    // 7 .. 11
    let expected = [112, 16, 133, 32, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorValueNotFound() throws {
    let source = """
errorValueNotFound
	self error: 'value not found'
"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveAssociationIfAbsent() throws {
    let source = """
removeAssociation: anAssociation ifAbsent: anExceptionBlock
	"Remove the key and value association, anAssociation, from the receiver.  If not found, answer the result of evaluating anExceptionBlock, otherwise answer anAssociation."

	^super remove: anAssociation ifAbsent: anExceptionBlock
"""
    // 7 .. 12
    let expected = [112, 16, 17, 133, 64, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveAssociation() throws {
    let source = """
removeAssociation: anAssociation
	"Remove the key and value association, anAssociation, from the receiver.  Answer anAssociation."

	^self removeAssociation: anAssociation ifAbsent: [self errorNotFound]
"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testFindKeyIfAbsent() throws {
    let source = """
findKey: key ifAbsent: aBlock
	| index |
	index _ self findKeyOrNil: key.
	(self basicAt: index) == nil ifTrue: [^aBlock value].
	^index
"""
    // 7 .. 21
    let expected = [112, 16, 224, 106, 112, 18, 225, 115, 198, 154, 17, 201, 124, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testErrorKeyNotFound() throws {
    let source = """
errorKeyNotFound
	self error: 'key not found'
"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testKeysDo() throws {
    let source = """
keysDo: aBlock
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association key]
"""
    // 7 .. 21
    let expected = [112, 137, 118, 200, 164, 6, 105, 16, 17, 209, 202, 125, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAssociationAtIfAbsent() throws {
    let source = """
associationAt: key ifAbsent: aBlock
	"Answer the association at key.  If key is not found, answer the
	result of evaluating aBlock."

	| index |
	index _ self findKey: key ifAbsent: [^aBlock value].
	^self basicAt: index
"""
    // 7 .. 22
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 106, 112, 18, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testKeyAtValueIfAbsent() throws {
    let source = """
keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key whose value equals the argument, value.  If there is none,
	answer the result of evaluating exceptionBlock."

	self associationsDo:
		[:association | value == association value ifTrue: [^association key]].
	^exceptionBlock value
"""
    // 7 .. 28
    let expected = [112, 137, 118, 200, 164, 11, 106, 16, 18, 201, 198, 154, 18, 209, 124, 115, 125, 224, 135, 17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testKeys() throws {
    let source = """
keys
	"Answer a set containing the receiver's keys."

	| aSet key |
	aSet _ Set new: self size.
	self keysDo: [:key | aSet add: key].
	^aSet
"""
    // 9 .. 28
    let expected = [64, 112, 194, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtIfAbsent() throws {
    let source = """
at: key ifAbsent: aBlock
	"Answer the value at key.  If key is not found, answer the
	result of evaluating aBlock."

	| index |
	index _ self findKey: key ifAbsent: [^aBlock value].
	^(self basicAt: index) value
"""
    // 7 .. 23
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 106, 112, 18, 225, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveKeyIfAbsent() throws {
    let source = """
removeKey: key ifAbsent: aBlock
	"Remove key from the receiver.  If key is not in the receiver,
	answer the result of evaluating aBlock.  Otherwise, answer the value associated
	with key."

	| index element |
	index _ self findKey: key ifAbsent: [^aBlock value].
	element _ self basicAt: index.
	self basicAt: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^element
"""
    // 11 .. 41
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 106, 112, 18, 225, 107, 112, 18, 115, 242, 135, 0, 118, 177, 96, 112, 18, 227, 135, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testAssociationsDo() throws {
    let source = """
associationsDo: aBlock
	"Evaluate aBlock for each of the receiver's key/value associations."

	super do: aBlock
"""
    // 7 .. 12
    let expected = [112, 16, 133, 32, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testValues() throws {
    let source = """
values
	"Answer a Bag containing the receiver's values."

	| aBag |
	aBag _ Bag new.
	self do: [:value | aBag add: value].
	^aBag
"""
    // 7 .. 24
    let expected = [64, 204, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 225, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testCollect() throws {
    let source = """
collect: aBlock
	"Evaluate aBlock with each of my values as the argument.  Collect the resulting
	values into a collection that is like me.  Answer with the new collection."

	| newCollection |
	newCollection _ Bag new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection
"""
    // 7 .. 26
    let expected = [64, 204, 105, 112, 137, 118, 200, 164, 7, 106, 17, 16, 18, 202, 225, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAssociations() throws {
    let source = """
associations
	"Answer an OrderedCollection containing the receiver's associations in an
	arbitrary order."

	| aCollection key |
	aCollection _ OrderedCollection new: self size.
	self associationsDo: [:key | aCollection add: key].
	^aCollection
"""
    // 9 .. 28
    let expected = [64, 112, 194, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testAssociationAt() throws {
    let source = """
associationAt: key
	"Answer the association at key.  If key is not found, create an error message."

	^self associationAt: key ifAbsent: [self errorKeyNotFound]
"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveKey() throws {
    let source = """
removeKey: key
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key."

	^self removeKey: key ifAbsent: [self errorKeyNotFound]
"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testIncludesKey() throws {
    let source = """
includesKey: key
	"Answer whether the receiver has a key equal to the argument, key."

	| index |
	index _ self findKeyOrNil: key.
	^(self basicAt: index) ~~ nil
"""
    // 9 .. 18
    let expected = [112, 16, 224, 105, 112, 17, 226, 115, 225, 124]
    try runningSource(source, expecting: expected)
  }

  func testKeyAtValue() throws {
    let source = """
keyAtValue: value
	"Answer the key whose value equals the argument, value.  If there is none,
	cause an error."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound]
"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: key
	"Answer the value at key.  If key is not found, create an error message."

	^self at: key ifAbsent: [self errorKeyNotFound]
"""
    // 7 .. 18
    let expected = [112, 16, 137, 117, 200, 164, 3, 112, 209, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testIncludes() throws {
    let source = """
includes: anObject
	self do: [:each | anObject = each ifTrue: [^true]].
	^false
"""
    // 3 .. 19
    let expected = [112, 137, 118, 200, 164, 8, 105, 16, 17, 182, 152, 121, 115, 125, 203, 135, 122]
    try runningSource(source, expecting: expected)
  }

  func testOccurrencesOf() throws {
    let source = """
occurrencesOf: anObject
	"Answer how many of the receiver's elements are equal to anObject."

	| count |
	count _ 0.
	self do: [:each | anObject = each ifTrue: [count _ count + 1]].
	^count
"""
    // 3 .. 27
    let expected = [117, 105, 112, 137, 118, 200, 164, 13, 106, 16, 18, 182, 157, 17, 118, 176, 129, 65, 144, 115, 125, 203, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: key put: anObject
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."

	| index element |
	index _ self findKeyOrNil: key.
	element _ self basicAt: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (Association key: key value: anObject)]
		ifFalse: [element value: anObject].
	^anObject
"""
    // 13 .. 38
    let expected = [112, 16, 224, 106, 112, 18, 225, 107, 19, 115, 198, 159, 112, 18, 68, 16, 17, 243, 242, 146, 19, 17, 202, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testRehash() throws {
    let source = """
rehash
	"Smalltalk rehash."

	| newSelf |
	newSelf _ self species new: self basicSize.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	self become: newSelf
"""
    // 13 .. 36
    let expected = [112, 208, 112, 209, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 227, 125, 226, 135, 112, 16, 228, 135, 120]
    try runningSource(source, expecting: expected)
  }
}
