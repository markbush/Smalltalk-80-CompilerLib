import XCTest
@testable import CompilerLib

final class CompileIntervalTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Interval", instanceVariables: ["start", "stop", "step"])
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

  func testLast() throws {
    let source = """
last
	^stop - (stop - start \\\\ step)

"""
    // 3 .. 10
    let expected = [1, 1, 0, 177, 2, 186, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetFromToBy() throws {
    let source = """
setFrom: startInteger to: stopInteger by: stepInteger
	start _ startInteger.
	stop _ stopInteger.
	step _ stepInteger

"""
    // 3 .. 9
    let expected = [16, 96, 17, 97, 18, 98, 120]
    try runningSource(source, expecting: expected)
  }

  func testSpecies() throws {
    let source = """
species
	^Array

"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }



  func testAdd() throws {
    let source = """
add: newObject
	"Adding to an Interval is not allowed."
	self shouldNotImplement

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"This is possible because we know numbers store and print the same"

	self printOn: aStream

"""
    // 5 .. 9
    let expected = [112, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	step < 0
		ifTrue: [start < stop
				ifTrue: [^0]
				ifFalse: [^stop - start // step + 1]]
		ifFalse: [stop < start
				ifTrue: [^0]
				ifFalse: [^stop - start // step + 1]]

"""
    // 3 .. 35
    let expected = [2, 117, 178, 172, 14, 0, 1, 178, 153, 117, 124, 1, 0, 177, 2, 189, 118, 176, 124, 1, 0, 178, 153, 117, 124, 1, 0, 177, 2, 189, 118, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPut: $(.
	start printOn: aStream.
	aStream nextPutAll: ' to: '.
	stop printOn: aStream.
	step ~= 1
		ifTrue:
			[aStream nextPutAll: ' by: '.
			step printOn: aStream].
	aStream nextPut: $)

"""
    // 15 .. 47
    let expected = [16, 32, 196, 135, 0, 16, 225, 135, 16, 35, 226, 135, 1, 16, 225, 135, 2, 118, 183, 159, 16, 36, 226, 135, 2, 16, 225, 135, 16, 37, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCollect() throws {
    let source = """
collect: aBlock
	| nextValue i result |
	result _ self species new: self size.
	nextValue _ start.
	i _ 1.
	step < 0
		ifTrue: [[stop <= nextValue]
				whileTrue:
					[result at: i put: (aBlock value: nextValue).
					nextValue _ nextValue + step.
					i _ i + 1]]
		ifFalse: [[stop >= nextValue]
				whileTrue:
					[result at: i put: (aBlock value: nextValue).
					nextValue _ nextValue + step.
					i _ i + 1]].
	^result

"""
    // 5 .. 70
    let expected = [112, 208, 112, 194, 205, 107, 0, 105, 118, 106, 2, 117, 178, 172, 25, 1, 17, 180, 172, 17, 19, 18, 16, 17, 202, 193, 135, 17, 2, 176, 105, 18, 118, 176, 106, 163, 234, 115, 164, 23, 1, 17, 181, 172, 17, 19, 18, 16, 17, 202, 193, 135, 17, 2, 176, 105, 18, 118, 176, 106, 163, 234, 115, 135, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: anInteger
	"Answer the anInteger'th element."

	(anInteger >= 1 and: [anInteger <= self size])
		ifTrue: [^start + (step * (anInteger - 1))]
		ifFalse: [self errorSubscriptBounds: anInteger]

"""
    // 5 .. 28
    let expected = [16, 118, 181, 156, 16, 112, 194, 180, 144, 114, 159, 0, 2, 16, 118, 177, 184, 176, 124, 112, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: anInteger put: anObject
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReverseDo() throws {
    let source = """
reverseDo: aBlock
	"Evaluate aBlock for each element of my interval, in reverse order."

	| aValue |
	aValue _ stop.
	step < 0
		ifTrue: [[start >= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue _ aValue - step]]
		ifFalse: [[start <= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue _ aValue - step]]

"""
    // 3 .. 45
    let expected = [1, 105, 2, 117, 178, 172, 18, 0, 17, 181, 172, 10, 16, 17, 202, 135, 17, 2, 177, 105, 163, 241, 115, 164, 16, 0, 17, 180, 172, 10, 16, 17, 202, 135, 17, 2, 177, 105, 163, 241, 115, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	^(((start hash bitShift: 2)
		bitOr: stop hash)
		bitShift: 1)
		bitOr: self size

"""
    // 5 .. 17
    let expected = [0, 208, 119, 188, 1, 208, 191, 118, 188, 112, 194, 191, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	| aValue |
	aValue _ start.
	step < 0
		ifTrue: [[stop <= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue _ aValue + step]]
		ifFalse: [[stop >= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue _ aValue + step]]

"""
    // 3 .. 45
    let expected = [0, 105, 2, 117, 178, 172, 18, 1, 17, 180, 172, 10, 16, 17, 202, 135, 17, 2, 176, 105, 163, 241, 115, 164, 16, 1, 17, 181, 172, 10, 16, 17, 202, 135, 17, 2, 176, 105, 163, 241, 115, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	"Return a copy of me.
	I override this message because my species is Array and copy, as inherited
	from SequenceableCollection, uses copyFrom:to:, which creates a new object
	of my species"

	^self shallowCopy

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemove() throws {
    let source = """
remove: newObject
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= anInterval
	"Answer true if my species and anInterval species are equal, and
	if our starts, steps and sizes are equal."

	self species == anInterval species
		ifTrue: [^start = anInterval first
					and: [step = anInterval increment and: [self size = anInterval size]]]
		ifFalse: [^false]

"""
    // 9 .. 37
    let expected = [112, 210, 16, 210, 198, 172, 21, 0, 16, 209, 182, 172, 13, 2, 16, 208, 182, 157, 112, 194, 16, 194, 182, 144, 114, 144, 114, 124, 122]
    try runningSource(source, expecting: expected)
  }

  func testHashMappedBy() throws {
    let source = """
hashMappedBy: map
	"My hash is independent of my oop"
	^ self hash

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
