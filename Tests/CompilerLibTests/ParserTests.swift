import XCTest
@testable import CompilerLib

final class ParserTests: XCTestCase {
  func testSimpleUnaryMethod() throws {
    let p = Parser(on: "yourself \"some comment\" ^self")
    let methodNode = try p.parseMethod()
    print(methodNode)
  }

  func testSimpleBinaryMethod() throws {
    let p = Parser(on: "+ aValue \"some comment\" ^self")
    let methodNode = try p.parseMethod()
    print(methodNode)
  }

  func testSimpleKeywordMethod() throws {
    let p = Parser(on: "value: aValue \"some comment\" ^self")
    let methodNode = try p.parseMethod()
    print(methodNode)
  }

  func testMultipleKeywordMethod() throws {
    let p = Parser(on: "value: aValue with: aNumber and: anotherValue \"some comment\" ^self")
    let methodNode = try p.parseMethod()
    print(methodNode)
  }
}
