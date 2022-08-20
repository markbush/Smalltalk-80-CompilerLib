import XCTest
@testable import CompilerLib

final class CompileRectangleTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Rectangle", instanceVariables: ["origin", "corner"])
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

  func testBottomLeft() throws {
    let source = """
bottomLeft
	"Answer the point at the left edge of the bottom horizontal line of the receiver."
	^origin x @ corner y

"""
    // 3 .. 8
    let expected = [0, 206, 1, 207, 187, 124]
    try runningSource(source, expecting: expected)
  }


  func testEquals() throws {
    let source = """
= aRectangle
	"Answer true if the receiver's species, origin and corner match aRectangle's."

	self species = aRectangle species
		ifTrue: [^origin = aRectangle origin and: [corner = aRectangle corner]]
		ifFalse: [^false]

"""
    // 9 .. 28
    let expected = [112, 210, 16, 210, 182, 172, 12, 0, 16, 209, 182, 156, 1, 16, 208, 182, 144, 114, 124, 122]
    try runningSource(source, expecting: expected)
  }

  func testExpandBy() throws {
    let source = """
expandBy: delta
	"Answer a Rectangle that is outset from the receiver by delta.
	 delta is a Rectangle, Point, or scalar."

	(delta isKindOf: Rectangle)
		ifTrue: [^Rectangle
					origin: origin - delta origin
					corner: corner + delta corner]
		ifFalse: [^Rectangle
					origin: origin - delta
					corner: corner + delta]

"""
    // 13 .. 37
    let expected = [16, 65, 228, 172, 11, 65, 0, 16, 210, 177, 1, 16, 211, 176, 240, 124, 65, 0, 16, 177, 1, 16, 176, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testOriginCorner() throws {
    let source = """
origin: originPoint corner: cornerPoint
	"Set the points at the top left corner and the bottom right corner of the receiver."
	origin _ originPoint.
	corner _ cornerPoint

"""
    // 3 .. 7
    let expected = [16, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testOriginExtent() throws {
    let source = """
origin: originPoint extent: extentPoint
	"Set the point at the top left corner of the receiver to be originPoint and
	set the width and height of the receiver to be extentPoint."
	origin _ originPoint.
	corner _ origin + extentPoint

"""
    // 3 .. 9
    let expected = [16, 96, 0, 17, 176, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testInsetBy() throws {
    let source = """
insetBy: delta
	"Answer a Rectangle that is inset from the receiver by delta.
	 delta is a Rectangle, Point, or scalar."

	(delta isKindOf: Rectangle)
		ifTrue: [^Rectangle
					origin: origin + delta origin
					corner: corner - delta corner]
		ifFalse: [^Rectangle
					origin: origin + delta
					corner: corner - delta]

"""
    // 13 .. 37
    let expected = [16, 65, 228, 172, 11, 65, 0, 16, 210, 176, 1, 16, 211, 177, 240, 124, 65, 0, 16, 176, 1, 16, 177, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetExtent() throws {
    let source = """
extent: extentPoint
	"Set the extent (width and height) of the receiver to be extentPoint."
	corner _ origin + extentPoint

"""
    // 3 .. 7
    let expected = [0, 16, 176, 97, 120]
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
    // 19 .. 49
    let expected = [16, 136, 32, 196, 135, 136, 112, 211, 210, 225, 135, 136, 36, 225, 135, 136, 0, 229, 135, 136, 38, 225, 135, 136, 1, 229, 135, 39, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }



  func testOrigin() throws {
    let source = """
origin: originPoint
	"Set the point at the top left corner of the receiver."
	origin _ originPoint

"""
    // 3 .. 5
    let expected = [16, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testCorner() throws {
    let source = """
corner: cornerPoint
	"Set the point at the bottom right corner of the receiver."
	corner _ cornerPoint

"""
    // 3 .. 5
    let expected = [16, 97, 120]
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

  func testSetRight() throws {
    let source = """
right: anInteger
	"Set the position of the receiver's right vertical line."
	corner x: anInteger

"""
    // 5 .. 9
    let expected = [1, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTopCenter() throws {
    let source = """
topCenter
	"Answer the point at the center of the receiver's top horizontal line."
	^self center x @ self top

"""
    // 7 .. 13
    let expected = [112, 208, 206, 112, 209, 187, 124]
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
    // 15 .. 76
    let expected = [117, 117, 187, 105, 112, 209, 16, 209, 178, 159, 17, 16, 209, 112, 209, 177, 224, 135, 112, 211, 16, 211, 178, 159, 17, 16, 211, 112, 211, 177, 226, 135, 112, 212, 16, 212, 179, 159, 17, 16, 212, 112, 212, 177, 224, 135, 112, 213, 16, 213, 179, 159, 17, 16, 213, 112, 213, 177, 226, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetLeft() throws {
    let source = """
left: anInteger
	"Set the position of the receiver's left vertical line."
	origin x: anInteger

"""
    // 5 .. 9
    let expected = [0, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMoveBy() throws {
    let source = """
moveBy: aPoint
	"Change the corner positions of the receiver so that its area translates by
	the amount defined by the argument, aPoint."
	origin _ origin + aPoint.
	corner _ corner + aPoint

"""
    // 3 .. 11
    let expected = [0, 16, 176, 96, 1, 16, 176, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testBottomCenter() throws {
    let source = """
bottomCenter
	"Answer the point at the center of the bottom horizontal line of the receiver."
	^self center x @ self bottom

"""
    // 7 .. 13
    let expected = [112, 208, 206, 112, 209, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testMerge() throws {
    let source = """
merge: aRectangle
	"Answer a Rectangle that contains both the receiver and aRectangle."

	^Rectangle
		origin: (origin min: aRectangle origin)
		corner: (corner max: aRectangle corner)

"""
    // 15 .. 25
    let expected = [65, 0, 16, 211, 226, 1, 16, 213, 228, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testLeftCenter() throws {
    let source = """
leftCenter
	"Answer the point at the center of the receiver's left vertical line."
	^self left @ self center y

"""
    // 7 .. 13
    let expected = [112, 208, 112, 209, 207, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testInsetOriginByCornerBy() throws {
    let source = """
insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint
	"Answer a Rectangle that is inset from the receiver by a given amount in the
	origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint

"""
    // 7 .. 15
    let expected = [65, 0, 16, 176, 1, 17, 177, 240, 124]
    try runningSource(source, expecting: expected)
  }


  func testBottom() throws {
    let source = """
bottom
	"Answer the position of the receiver's bottom horizontal line."
	^corner y

"""
    // 3 .. 5
    let expected = [1, 207, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetWidth() throws {
    let source = """
width: widthInteger
	"Change the receiver's right vertical line to make its width widthInteger."
	corner x: origin x + widthInteger

"""
    // 5 .. 12
    let expected = [1, 0, 206, 16, 176, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetTop() throws {
    let source = """
top: anInteger
	"Set the position of the receiver's top horizontal line."
	origin y: anInteger

"""
    // 5 .. 9
    let expected = [0, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTopLeft() throws {
    let source = """
topLeft: topLeftPoint
	"Set the point at the top left corner of the receiver's top horizontal line."
	origin _ topLeftPoint

"""
    // 3 .. 5
    let expected = [16, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testArea() throws {
    let source = """
area
	"Answer the receiver's area, the product of width and height."
	^self width * self height

"""
    // 7 .. 12
    let expected = [112, 208, 112, 209, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testRightCenter() throws {
    let source = """
rightCenter
	"Answer the point at the center of the receiver's right vertical line."
	^self right @ self center y

"""
    // 7 .. 13
    let expected = [112, 208, 112, 209, 207, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testBottomRight() throws {
    let source = """
bottomRight: bottomRightPoint
	"Set the position of the right corner of the bottom horizontal line of the receiver."
	corner _ bottomRightPoint

"""
    // 3 .. 5
    let expected = [16, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testContains() throws {
    let source = """
contains: aRectangle
	"Answer whether the receiver is equal to aRectangle or whether aRectangle
	is contained within the receiver."

	^aRectangle origin >= origin and: [aRectangle corner <= corner]

"""
    // 7 .. 18
    let expected = [16, 209, 0, 181, 156, 16, 208, 1, 180, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testIntersects() throws {
    let source = """
intersects: aRectangle
	"Answer whether aRectangle intersects the receiver anywhere."

	^(origin max: aRectangle origin) < (corner min: aRectangle corner)

"""
    // 11 .. 20
    let expected = [0, 16, 209, 224, 1, 16, 211, 226, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetHeight() throws {
    let source = """
height: heightInteger
	"Change the receiver's bottom y to make its height heightInteger."
	corner y: origin y + heightInteger

"""
    // 5 .. 12
    let expected = [1, 0, 207, 16, 176, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRight() throws {
    let source = """
right
	"Answer the position of the receiver's right vertical line."
	^corner x

"""
    // 3 .. 5
    let expected = [1, 206, 124]
    try runningSource(source, expecting: expected)
  }

  func testTopRight() throws {
    let source = """
topRight
	"Answer the point at the top right corner of the receiver's top horizontal line."
	^corner x @ origin y

"""
    // 3 .. 8
    let expected = [1, 206, 0, 207, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetBottom() throws {
    let source = """
bottom: anInteger
	"Set the position of the bottom horizontal line of the receiver."
	corner y: anInteger

"""
    // 5 .. 9
    let expected = [1, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMoveTo() throws {
    let source = """
moveTo: aPoint
	"Change the corners of the receiver so that its top left position is aPoint."

	corner _ corner + aPoint - origin.
	origin _ aPoint

"""
    // 3 .. 11
    let expected = [1, 16, 176, 0, 177, 97, 16, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testCenter() throws {
    let source = """
center
	"Answer the point at the center of the receiver."
	^self topLeft + self bottomRight // 2

"""
    // 7 .. 14
    let expected = [112, 208, 112, 209, 176, 119, 189, 124]
    try runningSource(source, expecting: expected)
  }

  func testContainsPoint() throws {
    let source = """
containsPoint: aPoint
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]

"""
    // 3 .. 12
    let expected = [0, 16, 180, 155, 16, 1, 178, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testAlignWith() throws {
    let source = """
align: aPoint1 with: aPoint2
	"Answer a new Rectangle that is a translated by aPoint2 - aPoint1."
	^self translateBy: aPoint2 - aPoint1

"""
    // 5 .. 10
    let expected = [112, 17, 16, 177, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testScaleBy() throws {
    let source = """
scaleBy: scale
	"Answer a new Rectangle scaled by scale, a Point or a scalar."

	^Rectangle origin: origin * scale corner: corner * scale

"""
    // 7 .. 15
    let expected = [65, 0, 16, 184, 1, 16, 184, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testTranslateBy() throws {
    let source = """
translateBy: factor
	"Answer a new Rectangle translated by factor, a Point or a scalar."

	^Rectangle origin: origin + factor corner: corner + factor

"""
    // 7 .. 15
    let expected = [65, 0, 16, 176, 1, 16, 176, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testRounded() throws {
    let source = """
rounded
	"Answer a Rectangle whose origin and corner are rounded."

	^Rectangle origin: origin rounded corner: corner rounded

"""
    // 9 .. 15
    let expected = [65, 0, 210, 1, 210, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testExtent() throws {
    let source = """
extent
	"Answer with a rectangle with origin 0@0 and corner the receiver's
	width @ the receiver's height."
	^corner - origin

"""
    // 3 .. 6
    let expected = [1, 0, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testLeft() throws {
    let source = """
left
	"Answer the position of the receiver's left vertical line."
	^origin x

"""
    // 3 .. 5
    let expected = [0, 206, 124]
    try runningSource(source, expecting: expected)
  }

  func testTop() throws {
    let source = """
top
	"Answer the position of the receiver's top horizontal line."
	^origin y

"""
    // 3 .. 5
    let expected = [0, 207, 124]
    try runningSource(source, expecting: expected)
  }

  func testWidth() throws {
    let source = """
width
	"Answer the width of the receiver."
	^corner x - origin x

"""
    // 3 .. 8
    let expected = [1, 206, 0, 206, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testHeight() throws {
    let source = """
height
	"Answer the height of the receiver."
	^corner y - origin y

"""
    // 3 .. 8
    let expected = [1, 207, 0, 207, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream

"""
    // 9 .. 21
    let expected = [0, 16, 224, 135, 16, 34, 225, 135, 1, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	^origin hash bitXor: corner hash

"""
    // 7 .. 12
    let expected = [0, 209, 1, 209, 224, 124]
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
    // 17 .. 132
    let expected = [0, 16, 211, 180, 156, 16, 210, 1, 180, 144, 114, 168, 4, 65, 112, 224, 124, 68, 204, 105, 16, 210, 207, 0, 207, 179, 172, 13, 17, 0, 1, 206, 16, 210, 207, 129, 66, 187, 230, 229, 147, 0, 207, 129, 66, 135, 16, 211, 207, 1, 207, 178, 172, 13, 17, 0, 206, 16, 211, 207, 129, 67, 187, 1, 230, 229, 147, 1, 207, 129, 67, 135, 16, 210, 206, 0, 206, 179, 172, 13, 17, 0, 206, 18, 187, 16, 210, 206, 19, 187, 230, 229, 135, 16, 211, 206, 1, 206, 178, 172, 13, 17, 16, 211, 206, 18, 187, 1, 206, 19, 187, 230, 229, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testIntersect() throws {
    let source = """
intersect: aRectangle
	"Answer a Rectangle that is the area in which the receiver overlaps with
	aRectangle. "

	^Rectangle
		origin: (origin max: aRectangle origin)
		corner: (corner min: aRectangle corner)

"""
    // 15 .. 25
    let expected = [65, 0, 16, 211, 226, 1, 16, 213, 228, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopy() throws {
    let source = """
copy
	^self deepCopy

"""
    // 5 .. 7
    let expected = [112, 208, 124]
    try runningSource(source, expecting: expected)
  }

}
