import XCTest
@testable import CompilerLib

final class CompileRectangleTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let rectangleClass = ClassDescription("Rectangle", instanceVariables: ["origin", "corner"])
    compiler = Compiler(forClass: rectangleClass)
  }
  override func tearDown() {
    compiler = nil
    super.tearDown()
  }
  func runningSource(_ source: String, expecting expected: [Bytecode]) throws {
    compiler.compileMethod(source)
    let actual = compiler.context.bytecodes
    XCTAssertEqual(actual.count, expected.count, "Unexpected number of bytecodes")
    let count = min(actual.count, expected.count)
    for i in 0..<count {
      XCTAssertEqual(actual[i], expected[i], "Different bytecodes at position \(i)")
    }
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

  func testOrigin() throws {
    let source = """
origin
	"Answer the point at the top left corner of the receiver."
	^origin
"""
    try runningSource(source, expecting: [.pushInstVar0, .returnTop])
  }

  func testSetExtent() throws {
    let source = """
extent: extentPoint
	"Set the extent (width and height) of the receiver to be extentPoint."
	corner _ origin + extentPoint
"""
    try runningSource(source, expecting: [.pushInstVar0, .pushTemporary0, .sendPlus, .popInstanceVar1, .returnSelf])
  }

  func testEquals() throws {
    let source = """
= aRectangle
	"Answer true if the receiver's species, origin and corner match aRectangle's."

	self species = aRectangle species
		ifTrue: [^origin = aRectangle origin and: [corner = aRectangle corner]]
		ifFalse: [^false]
"""
    compiler.context.literals = [.symbolConstant("corner"), .symbolConstant("origin"), .symbolConstant("species")]
    let expected = [112, 210, 16, 210, 182, 172, 12, 0, 16, 209, 182, 156, 1, 16, 208, 182, 144, 114, 124, 122]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream

	aStream nextPut: $(;
	nextPutAll: self species name;
	nextPutAll: ' origin: ';
	store: origin;
	nextPutAll: ' corner: ';
	store: corner;
	nextPut: $).
"""
    compiler.context.literals = [
      .characterConstant("("),
      .symbolConstant("nextPutAll:"),
      .symbolConstant("name"),
      .symbolConstant("species"),
      .stringConstant(" origin: "),
      .symbolConstant("store:"),
      .stringConstant(" corner: "),
      .characterConstant(")")
    ]
    let expected = [16, 136, 32, 196, 135, 136, 112, 211, 210, 225, 135, 136, 36, 225, 135, 136, 0, 229, 135, 136, 38, 225, 135, 136, 1, 229, 135, 39, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAmountToTranslateWithin() throws {
    let source = """
amountToTranslateWithin: aRectangle
	"Answer a Point, delta, such that self + delta is forced within aRectangle."

	| delta |
	delta _ 0@0.
	self left < aRectangle left ifTrue: [delta x: aRectangle left - self left].
	self top < aRectangle top ifTrue: [delta y: aRectangle top - self top].
	self right > aRectangle right ifTrue: [delta x: aRectangle right - self right].
	self bottom > aRectangle bottom ifTrue: [delta y: aRectangle bottom - self bottom].
	^delta
"""
    compiler.context.literals = [
      .symbolConstant("x:"),
      .symbolConstant("left"),
      .symbolConstant("y:"),
      .symbolConstant("top"),
      .symbolConstant("right"),
      .symbolConstant("bottom")
    ]
    let expected = [117, 117, 187, 105, 112, 209, 16, 209, 178, 159, 17, 16, 209, 112, 209, 177, 224, 135, 112, 211, 16, 211, 178, 159, 17, 16, 211, 112, 211, 177, 226, 135, 112, 212, 16, 212, 179, 159, 17, 16, 212, 112, 212, 177, 224, 135, 112, 213, 16, 213, 179, 159, 17, 16, 213, 112, 213, 177, 226, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAreasOutside() throws {
    let source = """
areasOutside: aRectangle
	"Answer with a Collection of Rectangles comprising the parts of me
	which do not lie within aRectangle."
	| areas yOrigin yCorner |
	"Make sure the intersection is non-empty"
	(origin <= aRectangle corner and: [aRectangle origin <= corner])
		ifFalse: [^Array with: self].
	areas _ OrderedCollection new.
	aRectangle origin y > origin y
		ifTrue: [areas add: (origin corner: corner x @ (yOrigin _ aRectangle origin y))]
		ifFalse: [yOrigin _ origin y].
	aRectangle corner y < corner y
		ifTrue: [areas add: (origin x @ (yCorner _ aRectangle corner y) corner: corner)]
		ifFalse: [yCorner _ corner y].
	aRectangle origin x > origin x
		ifTrue: [areas add: (origin x @ yOrigin corner: aRectangle origin x @ yCorner)].
	aRectangle corner x < corner x
		ifTrue: [areas add: (aRectangle corner x @ yOrigin corner: corner x @ yCorner)].
	^areas
"""
    compiler.context.literals = [
      .symbolConstant("with:"),
      .stringVariable("Array", "Array"),
      .symbolConstant("origin"),
      .symbolConstant("corner"),
      .stringVariable("OrderedCollection", "OrderedCollection"),
      .symbolConstant("add:"),
      .symbolConstant("corner:")
    ]
    let expected = [0, 16, 211, 180, 156, 16, 210, 1, 180, 144, 114, 168, 4, 65, 112, 224, 124, 68, 204, 105, 16, 210, 207, 0, 207, 179, 172, 13, 17, 0, 1, 206, 16, 210, 207, 129, 66, 187, 230, 229, 147, 0, 207, 129, 66, 135, 16, 211, 207, 1, 207, 178, 172, 13, 17, 0, 206, 16, 211, 207, 129, 67, 187, 1, 230, 229, 147, 1, 207, 129, 67, 135, 16, 210, 206, 0, 206, 179, 172, 13, 17, 0, 206, 18, 187, 16, 210, 206, 19, 187, 230, 229, 135, 16, 211, 206, 1, 206, 178, 172, 13, 17, 16, 211, 206, 18, 187, 1, 206, 19, 187, 230, 229, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }
}
