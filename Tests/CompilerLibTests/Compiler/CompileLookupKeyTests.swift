import XCTest
@testable import CompilerLib

final class CompileLookupKeyTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("LookupKey", instanceVariables: ["key"])
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


  func test2() throws {
    let source = """
hash
	^key hash
"""
    compiler.context.literals = [
      .symbolConstant("hash")
    ]
    // 5 .. 7
    let expected = [0, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func test3() throws {
    let source = """
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map"
	^ key hashMappedBy: map
"""
    compiler.context.literals = [
      .symbolConstant("hashMappedBy:")
    ]
    // 5 .. 8
    let expected = [0, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func test4() throws {
    let source = """
key: anObject
	"Store the argument, anObject, as the lookup key of the receiver."
	key _ anObject
"""
    compiler.context.literals = [

    ]
    // 3 .. 5
    let expected = [16, 96, 120]
    try runningSource(source, expecting: expected)
  }

  func test5() throws {
    let source = """
printOn: aStream
	key printOn: aStream
"""
    compiler.context.literals = [
      .symbolConstant("printOn:")
    ]
    // 5 .. 9
    let expected = [0, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func test6() throws {
    let source = """
< aLookupKey
	^key < aLookupKey key
"""
    compiler.context.literals = [
      .symbolConstant("key")
    ]
    // 5 .. 9
    let expected = [0, 16, 208, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func test7() throws {
    let source = """
= aLookupKey
	self species = aLookupKey species
		ifTrue: [^key = aLookupKey key]
		ifFalse: [^false]
"""
    compiler.context.literals = [
      .symbolConstant("key"),
      .symbolConstant("species")
    ]
    // 7 .. 18
    let expected = [112, 209, 16, 209, 182, 156, 0, 16, 208, 182, 124, 122]
    try runningSource(source, expecting: expected)
  }
}
