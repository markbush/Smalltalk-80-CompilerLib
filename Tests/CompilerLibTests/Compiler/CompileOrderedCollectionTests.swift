import XCTest
@testable import CompilerLib

final class CompileOrderedCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let orderedCollectionClass = ClassDescription("OrderedCollection", instanceVariables: ["firstIndex", "lastIndex"])
    compiler = Compiler(forClass: orderedCollectionClass)
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
    compiler.context.literals = [
      .symbolConstant("species"),
      .symbolConstant("reverseDo:"),
      .symbolConstant("add:")
    ]
    let expected = [112, 208, 204, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 225, 135, 16, 124]
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
    compiler.context.literals = [
      .symbolConstant("species"),
      .symbolConstant("add:")
    ]
    let expected = [112, 208, 112, 194, 205, 105, 112, 137, 118, 200, 164, 11, 106, 16, 18, 182, 153, 115, 146, 17, 18, 225, 125, 203, 135, 17, 124]
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
    compiler.context.literals = [
      .symbolConstant("basicAt:")
    ]
    let expected = [0, 105, 17, 1, 180, 172, 12, 16, 112, 17, 224, 202, 135, 17, 118, 176, 105, 163, 239, 120]
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
    compiler.context.literals = [
      .symbolConstant("basicAt:"),
      .symbolConstant("errorNotFound")
    ]
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
    compiler.context.literals = [
      .symbolConstant("errorOutOfBounds"),
      .symbolConstant("species"),
      .symbolConstant("to:do:"),
      .symbolConstant("add:")
    ]
    let expected = [117, 108, 16, 130, 72, 17, 130, 73, 16, 118, 178, 158, 117, 129, 73, 129, 72, 164, 39, 24, 112, 194, 179, 172, 10, 112, 194, 118, 176, 129, 73, 129, 72, 164, 23, 25, 24, 118, 177, 178, 153, 113, 147, 25, 112, 194, 179, 154, 112, 208, 135, 25, 24, 177, 118, 176, 129, 68, 135, 112, 209, 112, 194, 18, 194, 176, 20, 177, 205, 107, 118, 24, 118, 177, 137, 118, 200, 164, 7, 110, 19, 112, 22, 192, 227, 125, 242, 135, 118, 18, 194, 137, 118, 200, 164, 7, 110, 19, 18, 22, 192, 227, 125, 242, 135, 25, 118, 176, 112, 194, 137, 118, 200, 164, 7, 110, 19, 112, 22, 192, 227, 125, 242, 135, 19, 124]
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
    compiler.context.literals = [
      .symbolConstant("at:"),
      .symbolConstant("errorNoSuchElement"),
      .stringVariable("OrderedCollection", "OrderedCollection")
    ]
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
    compiler.context.literals = [
      .symbolConstant("truncated"),
      .symbolConstant("at:put:"),
      .symbolConstant("errorNoSuchElement"),
      .stringVariable("OrderedCollection", "OrderedCollection")
    ]
    let expected = [16, 208, 106, 18, 118, 178, 153, 113, 150, 18, 0, 176, 118, 177, 1, 179, 155, 112, 210, 164, 10, 112, 18, 0, 176, 118, 177, 17, 133, 65, 124, 135, 120]
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
    compiler.context.literals = [
      .symbolConstant("makeRoomAtFirst"),
      .symbolConstant("basicAt:put:"),
      .symbolConstant("basicAt:")
    ]
    let expected = [17, 108, 20, 0, 177, 107, 0, 118, 182, 158, 112, 208, 135, 0, 19, 176, 108, 0, 118, 177, 129, 0, 106, 18, 20, 118, 177, 178, 172, 15, 112, 18, 112, 18, 118, 176, 226, 241, 135, 18, 118, 176, 106, 163, 234, 112, 18, 16, 241, 135, 16, 124]
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
    compiler.context.literals = [
      .symbolConstant("species"),
      .symbolConstant("basicAt:"),
      .symbolConstant("add:"),
      .symbolConstant("removeIndex:")
    ]
    let expected = [112, 208, 204, 107, 0, 105, 17, 1, 180, 172, 24, 112, 17, 225, 106, 16, 18, 202, 159, 19, 18, 226, 135, 112, 17, 227, 148, 17, 118, 176, 129, 65, 135, 163, 227, 19, 124]
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
      compiler.context.literals = [
        .symbolConstant("removeIndex:"),
        .symbolConstant("basicAt:")
      ]
      let expected = [0, 106, 18, 1, 180, 172, 20, 16, 112, 18, 225, 182, 157, 112, 18, 224, 135, 16, 124, 18, 118, 176, 129, 66, 135, 163, 231, 17, 201, 124]
      try runningSource(source, expecting: expected)
    }

}
