import XCTest
@testable import CompilerLib

final class CompileMappedCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("MappedCollection", instanceVariables: ["domain", "map"])
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

  func testAdd() throws {
    let source = """
add: newObject
	self shouldNotImplement

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	aStream nextPut: $(.
	domain storeOn: aStream.
	aStream nextPutAll: ' mappedBy: '.
	map storeOn: aStream.
	aStream nextPut: $)

"""
    // 13 .. 33
    let expected = [16, 32, 196, 135, 0, 16, 225, 135, 16, 35, 226, 135, 1, 16, 225, 135, 16, 36, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	"This returns another MappedCollection whereas copyFrom:to: will return an
	object like my domain"

	^MappedCollection collection: domain map: map

"""
    // 7 .. 11
    let expected = [65, 0, 1, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testContents() throws {
    let source = """
contents
	"Answer the receiver's domain for mapping, a SequenceableCollection."
	^map collect: [:mappedIndex | domain at: mappedIndex]

"""
    // 5 .. 17
    let expected = [1, 137, 118, 200, 164, 5, 104, 0, 16, 192, 125, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSpecies() throws {
    let source = """
species
	^domain species

"""
    // 5 .. 7
    let expected = [0, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testSelect() throws {
    let source = """
select: aBlock
	| aStream |
	aStream _ WriteStream on: (self species new: self size).
	self do:
		[:domainValue |
		(aBlock value: domainValue)
			ifTrue: [aStream nextPut: domainValue]].
	^aStream contents

"""
    // 11 .. 40
    let expected = [65, 112, 210, 112, 194, 205, 224, 105, 112, 137, 118, 200, 164, 11, 106, 16, 18, 202, 155, 17, 18, 196, 144, 115, 125, 203, 135, 17, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testCollect() throws {
    let source = """
collect: aBlock
	| aStream |
	aStream _ WriteStream on: (self species new: self size).
	self do:
		[:domainValue |
		aStream nextPut: (aBlock value: domainValue)].
	^aStream contents

"""
    // 11 .. 36
    let expected = [65, 112, 210, 112, 194, 205, 224, 105, 112, 137, 118, 200, 164, 7, 106, 17, 16, 18, 202, 196, 125, 203, 135, 17, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testDo() throws {
    let source = """
do: aBlock
	map do:
		[:mapValue | aBlock value: (domain at: mapValue)]

"""
    // 3 .. 18
    let expected = [1, 137, 118, 200, 164, 7, 105, 16, 0, 17, 192, 202, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAt() throws {
    let source = """
at: anIndex
	^domain at: (map at: anIndex)

"""
    // 3 .. 8
    let expected = [0, 1, 16, 192, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: anIndex put: anObject
	^domain at: (map at: anIndex) put: anObject

"""
    // 3 .. 9
    let expected = [0, 1, 16, 192, 17, 193, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetCollectionMap() throws {
    let source = """
setCollection: aCollection map: aDictionary
	domain _ aCollection.
	map _ aDictionary

"""
    // 3 .. 7
    let expected = [16, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testSize() throws {
    let source = """
size
	^map size

"""
    // 3 .. 5
    let expected = [1, 194, 124]
    try runningSource(source, expecting: expected)
  }

}
