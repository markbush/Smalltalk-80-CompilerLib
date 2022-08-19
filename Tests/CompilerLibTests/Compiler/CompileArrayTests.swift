import XCTest
@testable import CompilerLib

final class CompileArrayTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Array", instanceVariables: [])
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

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"Use the literal form if possible."

	self isLiteral
		ifTrue:
			[aStream nextPut: $#; nextPut: $(.
			self do:
				[:element |
				element printOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]

"""
    compiler.context.literals = [
      .symbolConstant("storeOn:"),
      .characterConstant("#"),
      .characterConstant("("),
      .symbolConstant("printOn:"),
      .symbolConstant("space"),
      .characterConstant(")"),
      .symbolConstant("isLiteral"),
      .stringVariable("Array", "Array")
    ]
    // 19 .. 56
    let expected = [112, 214, 172, 28, 16, 136, 33, 196, 135, 34, 196, 135, 112, 137, 118, 200, 164, 8, 105, 17, 16, 227, 135, 16, 212, 125, 203, 135, 16, 37, 196, 147, 112, 16, 133, 32, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"make sure = arrays hash =ly"

	self size = 0 ifTrue: [^17171].
	^(self at: 1) hash + (self at: self size) hash

"""
    compiler.context.literals = [
      .intConstant("17171"),
      .symbolConstant("hash")
    ]
    // 7 .. 24
    let expected = [112, 194, 117, 182, 153, 32, 124, 112, 118, 192, 209, 112, 112, 194, 192, 209, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testHashMappedBy() throws {
    let source = """
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map"
	self size = 0 ifTrue: [^ self hash].
	^ (self first hashMappedBy: map) + (self last hashMappedBy: map)

"""
    compiler.context.literals = [
      .symbolConstant("hash"),
      .symbolConstant("hashMappedBy:"),
      .symbolConstant("first"),
      .symbolConstant("last")
    ]
    // 11 .. 28
    let expected = [112, 194, 117, 182, 154, 112, 208, 124, 112, 210, 16, 225, 112, 211, 16, 225, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testIsLiteral() throws {
    let source = """
isLiteral
	self detect: [:element | element isLiteral not] ifNone: [^true].
	^false

"""
    compiler.context.literals = [
      .symbolConstant("detect:ifNone:"),
      .symbolConstant("not"),
      .symbolConstant("isLiteral")
    ]
    // 9 .. 28
    let expected = [112, 137, 118, 200, 164, 5, 104, 16, 210, 209, 125, 137, 117, 200, 164, 1, 121, 240, 135, 122]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	| tooMany |
	tooMany _ aStream position + self maxPrint.
	aStream nextPut: $(.
	self do:
		[:element |
		aStream position > tooMany ifTrue: [aStream nextPutAll: '...etc...)'. ^self].
		element printOn: aStream.
		aStream space].
	aStream nextPut: $)

"""
    compiler.context.literals = [
      .symbolConstant("position"),
      .symbolConstant("maxPrint"),
      .characterConstant("("),
      .symbolConstant("nextPutAll:"),
      .stringConstant("...etc...)"),
      .symbolConstant("printOn:"),
      .symbolConstant("space"),
      .characterConstant(")")
    ]
    // 19 .. 59
    let expected = [16, 208, 112, 209, 176, 105, 16, 34, 196, 135, 112, 137, 118, 200, 164, 18, 106, 16, 208, 17, 179, 156, 16, 36, 227, 135, 120, 18, 16, 229, 135, 16, 214, 125, 203, 135, 16, 39, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
