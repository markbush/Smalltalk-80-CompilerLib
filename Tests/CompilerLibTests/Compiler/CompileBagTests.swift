import XCTest
@testable import CompilerLib

final class CompileBagTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Bag", instanceVariables: ["contents"])
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

  func testSortedCounts() throws {
    let source = """
sortedCounts
	"Answer with a collection of counts with elements, sorted by decreasing count."
	| counts |
	counts _ SortedCollection sortBlock: [:x :y | x >= y].
	contents associationsDo:
		[:assn |
		counts add: (Association key: assn value value: assn key)].
	^ counts

"""
    compiler.context.literals = [
      .symbolConstant("sortBlock:"),
      .stringVariable("SortedCollection", "SortedCollection"),
      .symbolConstant("associationsDo:"),
      .symbolConstant("add:"),
      .symbolConstant("key:value:"),
      .stringVariable("Association", "Association"),
      .symbolConstant("key")
    ]
    // 17 .. 50
    let expected = [65, 137, 119, 200, 164, 6, 106, 105, 17, 18, 181, 125, 224, 104, 0, 137, 118, 200, 164, 10, 107, 16, 69, 19, 201, 19, 214, 244, 227, 125, 226, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: oldObject ifAbsent: exceptionBlock
	| count |
	(self includes: oldObject)
		ifTrue: [(count _ contents at: oldObject) = 1
				ifTrue: [contents removeKey: oldObject]
				ifFalse: [contents at: oldObject put: count - 1]]
		ifFalse: [^exceptionBlock value].
	^oldObject

"""
    compiler.context.literals = [
      .symbolConstant("removeKey:"),
      .symbolConstant("includes:")
    ]
    // 7 .. 36
    let expected = [112, 16, 225, 172, 19, 0, 16, 192, 129, 66, 118, 182, 155, 0, 16, 224, 149, 0, 16, 18, 118, 177, 193, 146, 17, 201, 124, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIncludes() throws {
    let source = """
includes: anObject
	^contents includesKey: anObject

"""
    compiler.context.literals = [
      .symbolConstant("includesKey:")
    ]
    // 5 .. 8
    let expected = [0, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: newObject
	^self add: newObject withOccurrences: 1

"""
    compiler.context.literals = [
      .symbolConstant("add:withOccurrences:")
    ]
    // 5 .. 9
    let expected = [112, 16, 118, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddWithOccurrences() throws {
    let source = """
add: newObject withOccurrences: anInteger
	"Add the element newObject to the elements of the receiver.  Do so as
	though the element were added anInteger number of times.  Answer newObject."

	(self includes: newObject)
		ifTrue: [contents at: newObject put: anInteger + (contents at: newObject)]
		ifFalse: [contents at: newObject put: anInteger].
	^newObject

"""
    compiler.context.literals = [
      .symbolConstant("includes:")
    ]
    // 5 .. 25
    let expected = [112, 16, 224, 172, 9, 0, 16, 17, 0, 16, 192, 176, 193, 147, 0, 16, 17, 193, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]

"""
    compiler.context.literals = [
      .symbolConstant("associationsDo:"),
      .symbolConstant("timesRepeat:"),
      .symbolConstant("key")
    ]
    // 9 .. 32
    let expected = [0, 137, 118, 200, 164, 15, 105, 17, 201, 137, 117, 200, 164, 5, 16, 17, 210, 202, 125, 225, 125, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetDictionary() throws {
    let source = """
setDictionary
	contents _ Dictionary new

"""
    compiler.context.literals = [
      .stringVariable("Dictionary", "Dictionary")
    ]
    // 5 .. 8
    let expected = [64, 204, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: index
	self errorNotKeyed

"""
    compiler.context.literals = [
      .symbolConstant("errorNotKeyed")
    ]
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: index put: anObject
	self errorNotKeyed

"""
    compiler.context.literals = [
      .symbolConstant("errorNotKeyed")
    ]
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	| tally |
	tally _ 0.
	contents do: [:each | tally _ tally + each].
	^tally

"""
    compiler.context.literals = [

    ]
    // 3 .. 21
    let expected = [117, 104, 0, 137, 118, 200, 164, 7, 105, 16, 17, 176, 129, 64, 125, 203, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testOccurrencesOf() throws {
    let source = """
occurrencesOf: anObject
	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]

"""
    compiler.context.literals = [
      .symbolConstant("includes:")
    ]
    // 5 .. 14
    let expected = [112, 16, 224, 155, 0, 16, 192, 124, 117, 124]
    try runningSource(source, expecting: expected)
  }

  func testSortedElements() throws {
    let source = """
sortedElements
	"Answer with a collection of elements with counts, sorted by element."
	| elements |
	elements _ SortedCollection new.
	contents associationsDo: [:assn | elements add: assn].
	^ elements

"""
    compiler.context.literals = [
      .stringVariable("SortedCollection", "SortedCollection"),
      .symbolConstant("associationsDo:"),
      .symbolConstant("add:")
    ]
    // 9 .. 26
    let expected = [64, 204, 104, 0, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 225, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

}
