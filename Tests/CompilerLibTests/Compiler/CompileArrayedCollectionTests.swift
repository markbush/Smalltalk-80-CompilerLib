import XCTest
@testable import CompilerLib

final class CompileArrayedCollectionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("ArrayedCollection", instanceVariables: [])
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

  func testSize() throws {
    let source = """
size
	"Answer the number of indexable fields in the receiver. This value is the
	same as the largest legal subscript. Primitive is specified here to override
	SequenceableCollection size. Essential. See Object documentation
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize

"""
    // 9 .. 11
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreElementsFromToOn() throws {
    let source = """
storeElementsFrom: firstIndex to: lastIndex on: aStream
	| noneYet defaultElement arrayElement |
	noneYet _ true.
	defaultElement _ self defaultElement.
	firstIndex to: lastIndex do:
		[:index |
		arrayElement _ self at: index.
		arrayElement = defaultElement
			ifFalse:
				[noneYet
					ifTrue: [noneYet _ false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet

"""
    // 17 .. 70
    let expected = [113, 107, 112, 208, 108, 16, 17, 137, 118, 200, 164, 38, 110, 112, 22, 192, 109, 21, 20, 182, 154, 115, 164, 25, 19, 155, 114, 129, 67, 146, 18, 34, 196, 135, 18, 36, 227, 135, 18, 22, 229, 135, 18, 38, 227, 135, 18, 21, 229, 125, 241, 135, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testDefaultElement() throws {
    let source = """
defaultElement
	^nil

"""
    // 3 .. 3
    let expected = [123]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)

"""
    // 19 .. 59
    let expected = [16, 33, 224, 135, 16, 112, 199, 210, 224, 135, 16, 35, 224, 135, 16, 112, 194, 228, 135, 16, 37, 196, 135, 112, 118, 112, 194, 16, 131, 103, 168, 4, 16, 38, 224, 135, 16, 37, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
