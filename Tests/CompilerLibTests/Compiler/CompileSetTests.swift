import XCTest
@testable import CompilerLib

final class CompileSetTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Set", instanceVariables: ["tally"])
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

  func testFixCollisionsFrom() throws {
    // backslash doubled in quoted string!
    let source = """
fixCollisionsFrom: index
	| myLength oldIndex nextIndex nextObject |
	oldIndex _ index.
	myLength _ self basicSize.
	[oldIndex _ oldIndex \\\\ myLength + 1.
	nextObject _ self basicAt: oldIndex.
	nextObject == nil]
		whileFalse:
			[nextIndex _ self findElementOrNil: nextObject.
			nextIndex = oldIndex
				ifFalse:
					[self basicAt: nextIndex put: nextObject.
					self basicAt: oldIndex put: nil]]
"""
    // 11 .. 52
    let expected = [16, 106, 112, 208, 105, 18, 17, 186, 118, 176, 106, 112, 18, 227, 108, 20, 115, 198, 168, 21, 112, 20, 225, 107, 19, 18, 182, 168, 10, 112, 19, 20, 242, 135, 112, 18, 115, 242, 135, 163, 220, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtNewIndexPut() throws {
    let source = """
atNewIndex: index put: anObject
	self basicAt: index put: anObject.
	tally _ tally + 1.
	self fullCheck
"""
    // 7 .. 19
    let expected = [112, 16, 17, 240, 135, 0, 118, 176, 96, 112, 209, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNoCheckAdd() throws {
    let source = """
noCheckAdd: anObject
	self basicAt: (self findElementOrNil: anObject)
		put: anObject.
	tally _ tally + 1
"""
    // 7 .. 18
    let expected = [112, 112, 16, 225, 16, 240, 135, 0, 118, 176, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testRehash() throws {
    let source = """
rehash
	| newSelf |
	newSelf _ self species new: self basicSize.
	self do: [:each | newSelf noCheckAdd: each].
	self become: newSelf
"""
    // 11 .. 34
    let expected = [112, 208, 112, 209, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 226, 125, 203, 135, 112, 16, 227, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFindIfAbsent() throws {
    let source = """
find: anObject ifAbsent: aBlock
	| index |
	index _ self findElementOrNil: anObject.
	(self basicAt: index) == nil
		ifTrue: [^aBlock value]
		ifFalse: [^index]
"""
    // 7 .. 21
    let expected = [112, 16, 224, 106, 112, 18, 225, 115, 198, 154, 17, 201, 124, 18, 124]
    try runningSource(source, expecting: expected)
  }

  func testFullCheck() throws {
    let source = """
fullCheck
	self basicSize - self size <= (self basicSize // 4) ifTrue: [self grow]
"""
    // 9 .. 23
    let expected = [112, 209, 112, 194, 177, 112, 209, 34, 189, 180, 154, 112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFindElementOrNil() throws {
    // backslash doubled in quoted string!
    let source = """
findElementOrNil: anObject
	| index length probe pass |
	length _ self basicSize.
	pass _ 1.
	index _ anObject hash \\\\ length + 1.
	[(probe _ self basicAt: index) == nil or: [probe = anObject]]
		whileFalse: [(index _ index + 1) > length
				ifTrue:
					[index _ 1.
					pass _ pass + 1.
					pass > 2 ifTrue: [^self grow findElementOrNil: anObject]]].
	^index
"""
    // 13 .. 67
    let expected = [112, 208, 106, 118, 108, 16, 209, 18, 186, 118, 176, 105, 112, 17, 228, 129, 67, 115, 198, 153, 113, 146, 19, 16, 182, 168, 26, 17, 118, 176, 129, 65, 18, 179, 172, 15, 118, 105, 20, 118, 176, 108, 20, 119, 179, 156, 112, 211, 16, 226, 124, 163, 215, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetTally() throws {
    let source = """
setTally
	tally _ 0
"""
    // 3 .. 5
    let expected = [117, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: newObject
	| index |
	newObject == nil ifTrue: [^newObject].
	index _ self findElementOrNil: newObject.
	(self basicAt: index) == nil ifTrue: [self atNewIndex: index put: newObject].
	^newObject
"""
    // 9 .. 31
    let expected = [16, 115, 198, 153, 16, 124, 112, 16, 224, 105, 112, 17, 226, 115, 198, 156, 112, 17, 16, 241, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }


  func testCollect() throws {
    let source = """
collect: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Collect the
	resulting values into another Set.  Answer the new Set.  We override the general
	method, so that we make a big enough set and avoid growing. "

	| newSet size index element |
	tally = 0 ifTrue: [^Set new: 2].
	newSet _ Set new: (size _ self basicSize).
	index _ 0.
	[(index _ index + 1) <= size] whileTrue:
		[(element _ self basicAt: index) == nil ifFalse:
			[newSet add: (aBlock value: element)]].
	^newSet
"""
    // 11 .. 55
    let expected = [0, 117, 182, 155, 64, 119, 205, 124, 64, 112, 209, 129, 66, 205, 105, 117, 107, 19, 118, 176, 129, 67, 18, 180, 172, 17, 112, 19, 227, 129, 68, 115, 198, 168, 6, 17, 16, 20, 202, 226, 135, 163, 230, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testSwapWith() throws {
    let source = """
swap: oneElement with: otherElement
	| save |
	save _ self basicAt: oneElement.
	self basicAt: oneElement put: (self basicAt: otherElement).
	self basicAt: otherElement put: save
"""
    // 7 .. 23
    let expected = [112, 16, 224, 106, 112, 16, 112, 17, 224, 241, 135, 112, 17, 18, 241, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testGrow() throws {
    let source = """
grow
	"The receiver becomes twice as big--this is not a copy of the receiver, so all shared references survive."

	| newSelf |
	newSelf _ self species new: self basicSize + self growSize.
	self do: [:each | newSelf noCheckAdd: each].
	self become: newSelf
"""
    // 13 .. 39
    let expected = [112, 208, 112, 209, 112, 210, 176, 205, 104, 112, 137, 118, 200, 164, 5, 105, 16, 17, 227, 125, 203, 135, 112, 16, 228, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: index
	self errorNotKeyed
"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: index put: anObject
	self errorNotKeyed
"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOccurrencesOf() throws {
    let source = """
occurrencesOf: anObject
	(self includes: anObject)
		ifTrue: [^1]
		ifFalse: [^0]
"""
    // 5 .. 12
    let expected = [112, 16, 224, 153, 118, 124, 117, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: oldObject ifAbsent: aBlock
	| index |
	index _ self find: oldObject ifAbsent: [^aBlock value].
	self basicAt: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^oldObject
"""
    // 9 .. 35
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 106, 112, 18, 115, 241, 135, 0, 118, 177, 96, 112, 18, 226, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testIncludes() throws {
    let source = """
includes: anObject
	^(self basicAt: (self findElementOrNil: anObject)) ~~ nil
"""
    // 9 .. 16
    let expected = [112, 112, 16, 226, 225, 115, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	tally = 0 ifTrue: [^self].
	1 to: self basicSize do:
		[:index |
		(self basicAt: index) == nil ifFalse: [aBlock value: (self basicAt: index)]]
"""
    // 9 .. 39
    let expected = [0, 117, 182, 152, 120, 118, 112, 209, 137, 118, 200, 164, 15, 105, 112, 17, 226, 115, 198, 153, 115, 148, 16, 112, 17, 226, 202, 125, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }
}
