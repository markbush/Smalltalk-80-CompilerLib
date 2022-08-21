import XCTest
@testable import CompilerLib

final class CompilePointTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Point", instanceVariables: ["x", "y"])
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

  func testExtent() throws {
    let source = """
extent: aPoint
	"Answer a new Rectangle whose origin is the receiver and whose extent is aPoint.
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self extent: aPoint

"""
    // 7 .. 11
    let expected = [65, 112, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testGrid() throws {
    let source = """
grid: aPoint
	"Answer a new Point to the nearest rounded grid modules specified
	by aPoint."

	| newX newY |

	aPoint x = 0
		ifTrue:	[newX _ 0]
		ifFalse:	[newX _ x roundTo: aPoint x].
	aPoint y = 0
		ifTrue:	[newY _ 0]
		ifFalse:	[newY _ y roundTo: aPoint y].
	^newX @ newY

"""
    // 5 .. 40
    let expected = [16, 206, 117, 182, 155, 117, 129, 65, 149, 0, 16, 206, 224, 129, 65, 135, 16, 207, 117, 182, 155, 117, 129, 66, 149, 1, 16, 207, 224, 129, 66, 135, 17, 18, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testTruncateTo() throws {
    let source = """
truncateTo: grid
	"Answer a new Point that is the receiver's x and y truncated to grid x and grid y."

	^(x truncateTo: grid) @ (y truncateTo: grid)

"""
    // 5 .. 12
    let expected = [0, 16, 224, 1, 16, 224, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y printOn: aStream

"""
    // 7 .. 19
    let expected = [0, 16, 224, 135, 16, 33, 196, 135, 1, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testY() throws {
    let source = """
y: yInteger
	"Set the y coordinate."
	y _ yInteger

"""
    // 3 .. 5
    let expected = [16, 97, 120]
    try runningSource(source, expecting: expected)
  }


  func testLessThan() throws {
    let source = """
< aPoint
	"Answer whether the receiver is 'above and to the left' of aPoint."
	^x < aPoint x and: [y < aPoint y]

"""
    // 3 .. 14
    let expected = [0, 16, 206, 178, 156, 1, 16, 207, 178, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testX() throws {
    let source = """
x: xInteger
	"Set the x coordinate."
	x _ xInteger

"""
    // 3 .. 5
    let expected = [16, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func testLessThanOrEquals() throws {
    let source = """
<= aPoint
	"Answer whether the receiver is 'neither below nor to the right' of aPoint."

	^x <= aPoint x and: [y <= aPoint y]

"""
    // 3 .. 14
    let expected = [0, 16, 206, 180, 156, 1, 16, 207, 180, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThanOrEquals() throws {
    let source = """
>= aPoint
	"Answer whether the receiver is 'neither above nor to the left' of aPoint."

	^x >= aPoint x and: [y >= aPoint y]

"""
    // 3 .. 14
    let expected = [0, 16, 206, 181, 156, 1, 16, 207, 181, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testGreaterThan() throws {
    let source = """
> aPoint
	"Answer whether the receiver is 'below and to the right' of aPoint."

	^x > aPoint x and: [y > aPoint y]

"""
    // 3 .. 14
    let expected = [0, 16, 206, 179, 156, 1, 16, 207, 179, 144, 114, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	^(x hash bitShift: 2) bitXor: y hash

"""
    // 7 .. 14
    let expected = [0, 209, 119, 188, 1, 209, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testCorner() throws {
    let source = """
corner: aPoint
	"Answer a new Rectangle whose origin is the receiver and whose corner is aPoint.
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self corner: aPoint

"""
    // 7 .. 11
    let expected = [65, 112, 16, 240, 124]
    try runningSource(source, expecting: expected)
  }


  func testDotProduct() throws {
    let source = """
dotProduct: aPoint
	"Answer a Number that is the dot product of the receiver and the argument, aPoint.
	That is, the two points are multipled and the coordinates of the result summed."

	| temp |
	temp _ self * aPoint.
	^temp x abs + temp y abs

"""
    // 5 .. 16
    let expected = [112, 16, 184, 105, 17, 206, 208, 17, 207, 208, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetXSetY() throws {
    let source = """
setX: xPoint setY: yPoint
	x _ xPoint.
	y _ yPoint

"""
    // 3 .. 7
    let expected = [16, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testEquals() throws {
    let source = """
= aPoint
	self species = aPoint species
		ifTrue: [^x = aPoint x and: [y = aPoint y]]
		ifFalse: [^false]

"""
    // 5 .. 24
    let expected = [112, 208, 16, 208, 182, 172, 12, 0, 16, 206, 182, 156, 1, 16, 207, 182, 144, 114, 124, 122]
    try runningSource(source, expecting: expected)
  }

  func testAbs() throws {
    let source = """
abs
	"Answer a new Point whose x and y are the absolute values of the receiver's
	x and y."

	^Point x: x abs y: y abs

"""
    // 9 .. 15
    let expected = [65, 0, 210, 1, 210, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testDeepCopy() throws {
    let source = """
deepCopy
	"Implemented here for better performance."
	^x deepCopy @ y deepCopy

"""
    // 5 .. 10
    let expected = [0, 208, 1, 208, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testTheta() throws {
    let source = """
theta
	"Answer the angle the receiver makes with origin in radians.
	right is 0; down is 90."

	| tan theta |
	x = 0
		ifTrue: [y >= 0
				ifTrue: [^1.5708"90.0 degreesToRadians"]
				ifFalse: [^4.71239"270.0 degreesToRadians"]]
		ifFalse:
			[tan _ y asFloat / x asFloat.
			theta _ tan arcTan.
			x >= 0
				ifTrue: [y >= 0
						ifTrue: [^theta]
						ifFalse: [^360.0 degreesToRadians + theta]]
				ifFalse: [^180.0 degreesToRadians + theta]]

"""
    // 17 .. 58
    let expected = [0, 117, 182, 159, 1, 117, 181, 153, 38, 124, 37, 124, 1, 208, 0, 208, 185, 104, 16, 209, 105, 0, 117, 181, 172, 11, 1, 117, 181, 153, 17, 124, 36, 210, 17, 176, 124, 35, 210, 17, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testR() throws {
    let source = """
r
	"Answer the receiver's radius in polar coordinate system."

	^(self dotProduct: self) sqrt

"""
    // 7 .. 11
    let expected = [112, 112, 225, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testUnitVector() throws {
    let source = """
unitVector
	"Answer the receiver scaled to unit length."
	^self / self r

"""
    // 5 .. 9
    let expected = [112, 112, 208, 185, 124]
    try runningSource(source, expecting: expected)
  }


  func testPointNearestLineTo() throws {
    let source = """
pointNearestLine: point1 to: point2
	"Answers the closest integer point to the receiver on the line determined by (point1, point2)."

	| relPoint delta |
	delta _ point2 - point1. 			"normalize coordinates"
	relPoint _ self - point1.
	delta x = 0 ifTrue: [^point1 x@y].
	delta y = 0 ifTrue: [^x@point1 y].
	delta x abs > delta y abs 		"line more horizontal?"
		ifTrue: [^x@(point1 y + (x * delta y // delta x))]
		ifFalse: [^(point1 x + (relPoint y * delta x // delta y))@y]

	"43@55 pointNearestLine: 10@10 to: 100@200"

"""
    // 5 .. 68
    let expected = [17, 16, 177, 107, 112, 16, 177, 106, 19, 206, 117, 182, 156, 16, 206, 1, 187, 124, 19, 207, 117, 182, 156, 0, 16, 207, 187, 124, 19, 206, 208, 19, 207, 208, 179, 172, 13, 0, 16, 207, 0, 19, 207, 184, 19, 206, 189, 176, 187, 124, 16, 206, 18, 207, 19, 206, 184, 19, 207, 189, 176, 1, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testTranspose() throws {
    let source = """
transpose
	"Answer a new Point whose x is the receiver's y and whose y is the receiver's x."

	^y @ x

"""
    // 3 .. 6
    let expected = [1, 0, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testDiv() throws {
    let source = """
// scale
	"Answer a new Point that is the quotient of the receiver and scale (which is a
	Point or Number)."

	| scalePoint |
	scalePoint _ scale asPoint.
	^x // scalePoint x @ (y // scalePoint y)
"""
    // 5 .. 17
    let expected = [16, 208, 105, 0, 17, 206, 189, 1, 17, 207, 189, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testPlus() throws {
    let source = """
+ delta
	"Answer a new Point that is the sum of the receiver and delta (which is a Point
	or Number)."

	| deltaPoint |
	deltaPoint _ delta asPoint.
	^x + deltaPoint x @ (y + deltaPoint y)

"""
    // 5 .. 17
    let expected = [16, 208, 105, 0, 17, 206, 176, 1, 17, 207, 176, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testTruncatedGrid() throws {
    let source = """
truncatedGrid: aPoint
	"Answer a new Point to the nearest truncated grid modules specified
	by aPoint."

	^(x truncateTo: aPoint x) @ (y truncateTo: aPoint y)

"""
    // 5 .. 14
    let expected = [0, 16, 206, 224, 1, 16, 207, 224, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testNormal() throws {
    let source = """
normal
	"Answer a new Point representing the unit vector rotated 90 deg toward the y axis."

	^(y negated @ x) unitVector

"""
    // 7 .. 12
    let expected = [1, 209, 0, 187, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testDist() throws {
    let source = """
dist: aPoint
	"Answer the distance between aPoint and the receiver."

	^(aPoint - self) r

"""
    // 5 .. 9
    let expected = [16, 112, 177, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testMultiply() throws {
    let source = """
* scale
	"Answer a new Point that is the product of the receiver and scale (which is a
	Point or Number)."

	| scalePoint |
	scalePoint _ scale asPoint.
	^x * scalePoint x @ (y * scalePoint y)

"""
    // 5 .. 17
    let expected = [16, 208, 105, 0, 17, 206, 184, 1, 17, 207, 184, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream

	aStream nextPut: $(;
	nextPutAll: self species name;
	nextPutAll: ' x: ';
	store: x;
	nextPutAll: ' y: ';
	store: y;
	nextPut: $).

"""
    // 19 .. 49
    let expected = [16, 136, 32, 196, 135, 136, 112, 211, 210, 225, 135, 136, 36, 225, 135, 136, 0, 229, 135, 136, 38, 225, 135, 136, 1, 229, 135, 39, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testDivide() throws {
    let source = """
/ scale
	"Answer a new Point that is the quotient of the receiver and scale (which is a
	Point or Number)."

	| scalePoint |
	scalePoint _ scale asPoint.
	^x / scalePoint x @ (y / scalePoint y)
"""
    // 5 .. 17
    let expected = [16, 208, 105, 0, 17, 206, 185, 1, 17, 207, 185, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testShallowCopy() throws {
    let source = """
shallowCopy
	"Implemented here for better performance."
	^x @ y

"""
    // 3 .. 6
    let expected = [0, 1, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testGenerality() throws {
    let source = """
generality
	^90

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testScaleBy() throws {
    let source = """
scaleBy: factor
	"Answer a new Point scaled by factor (an instance of Point)."

	^(factor x * x) @ (factor y * y)

"""
    // 3 .. 12
    let expected = [16, 206, 0, 184, 16, 207, 1, 184, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testTranslateBy() throws {
    let source = """
translateBy: delta
	"Answer a new Point translated by delta (an instance of Point)."

	^(delta x + x) @ (delta y + y)

"""
    // 3 .. 12
    let expected = [16, 206, 0, 176, 16, 207, 1, 176, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testCoerce() throws {
    let source = """
coerce: aNumber
	^aNumber@aNumber

"""
    // 3 .. 6
    let expected = [16, 16, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testMinus() throws {
    let source = """
- delta
	"Answer a new Point that is the difference of the receiver and delta (which is a
	Point or Number)."

	| deltaPoint |
	deltaPoint _ delta asPoint.
	^x - deltaPoint x @ (y - deltaPoint y)

"""
    // 5 .. 17
    let expected = [16, 208, 105, 0, 17, 206, 177, 1, 17, 207, 177, 187, 124]
    try runningSource(source, expecting: expected)
  }

  func testRounded() throws {
    let source = """
rounded
	"Answer a new Point that is the receiver's x and y rounded."

	^x rounded @ y rounded

"""
    // 5 .. 10
    let expected = [0, 208, 1, 208, 187, 124]
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

  func testMin() throws {
    let source = """
min: aPoint
	"Answer the upper left corner of the rectangle uniquely defined
	by the receiver and aPoint."

	^Point
		x: (x min: aPoint x)
		y: (y min: aPoint y)

"""
    // 9 .. 19
    let expected = [65, 0, 16, 206, 226, 1, 16, 207, 226, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testMax() throws {
    let source = """
max: aPoint
	"Answer the lower right corner of the rectangle uniquely defined
	by the receiver and aPoint."

	^Point
		x: (x max: aPoint x)
		y: (y max: aPoint y)

"""
    // 9 .. 19
    let expected = [65, 0, 16, 206, 226, 1, 16, 207, 226, 240, 124]
    try runningSource(source, expecting: expected)
  }

}
