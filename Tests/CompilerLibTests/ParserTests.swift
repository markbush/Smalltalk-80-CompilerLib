import XCTest
@testable import CompilerLib

final class ParserTests: XCTestCase {
  func testSimpleUnaryMethod() throws {
    let p = Parser(on: "yourself \"some comment\" ^self")
    let _ = try p.parseMethod()
  }

  func testSimpleBinaryMethod() throws {
    let p = Parser(on: "+ aValue \"some comment\" ^self")
    let _ = try p.parseMethod()
  }

  func testSimpleKeywordMethod() throws {
    let p = Parser(on: "value: aValue \"some comment\" ^self")
    let _ = try p.parseMethod()
  }

  func testMultipleKeywordMethod() throws {
    let p = Parser(on: "value: aValue with: aNumber and: anotherValue \"some comment\" ^self")
    let _ = try p.parseMethod()
  }
}
