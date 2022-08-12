import XCTest
@testable import CompilerLib

final class ScannerTests: XCTestCase {
  func testEmpty() throws {
    let scanner = Scanner(on: "")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .eof)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 0)
  }

  func testIdentifier() throws {
    let scanner = Scanner(on: "aVar other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .identifier)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "aVar")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 5)
  }

  func testIdentifierWithComments() throws {
    let scanner = Scanner(on: " \"a comment\" aVar \"another comment\" other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.comments.count, 2)
    XCTAssertEqual(token.comments[0], "a comment")
    XCTAssertEqual(token.comments[1], "another comment")
    XCTAssertEqual(token.type, .identifier)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "aVar")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 13)
    XCTAssertEqual(scanner.position, 36)
  }

  func testCommentWithEmbeddedSingleQuote() throws {
    let scanner = Scanner(on: " \"a 'comment\" aVar ")
    XCTAssertEqual(scanner.comments.count, 1)
    XCTAssertEqual(scanner.comments[0], "a 'comment")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .identifier)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "aVar")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 14)
    XCTAssertEqual(scanner.position, 19)
  }

  func testCommentWithEmbeddedDoubleQuote() throws {
    let scanner = Scanner(on: " \"a \"\"comment\" aVar ")
    XCTAssertEqual(scanner.comments.count, 1)
    XCTAssertEqual(scanner.comments[0], "a \"comment")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .identifier)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "aVar")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 15)
    XCTAssertEqual(scanner.position, 20)
  }

  func testIdentifierWithWhitespace() throws {
    let scanner = Scanner(on: "  aVar other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .identifier)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "aVar")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 2)
    XCTAssertEqual(scanner.position, 7)
  }

  func testIdentifierWithNumbers() throws {
    let scanner = Scanner(on: "a0Var123 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .identifier)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "a0Var123")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 9)
  }

  func testKeyword() throws {
    let scanner = Scanner(on: "aKeyword: other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .keyword)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "aKeyword:")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 10)
  }

  func testBlockArg() throws {
    let scanner = Scanner(on: ":anArg123 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .blockArg)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "anArg123")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 10)
  }

  func testBlockArgStartingWithNumber() throws {
    let scanner = Scanner(on: ":1anArg other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .error)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "an identifier must start with a letter")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 1)
  }

  func testAssign() throws {
    let scanner = Scanner(on: ":= other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .assign)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 3)
  }

  func testBinary() throws {
    let binaryChars = ["+", "/", "\\", "*", "~", "<", ">", "=", "@", "%", "|", "&", "?", "!"]
    var binarySelectors = ["-"]
    for char1 in binaryChars {
      binarySelectors.append(char1)
      for char2 in binaryChars {
        binarySelectors.append(char1+char2)
      }
    }
    for selector in binarySelectors {
      let selectorString = selector + " other stuff"
      let scanner = Scanner(on: selectorString)
      let token = scanner.nextToken()
      XCTAssertEqual(token.type, .binary)
      if let valueToken = token as? ValueToken {
        XCTAssertEqual(valueToken.value, selector)
      } else {
        XCTFail("Expected ValueToken!")
      }
      XCTAssertEqual(token.position, 0)
      XCTAssertEqual(scanner.position, selector.count+1)
    }
  }

  func testString() throws {
    let scanner = Scanner(on: "'a string' other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .string)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "a string")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 11)
  }

  func testIncompleteString() throws {
    let scanner = Scanner(on: "'a string other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .error)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "unmatched \"'\"")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 21)
  }

  func testStringWithEmbeddedQuote() throws {
    let scanner = Scanner(on: "'a string'' with a quote' other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .string)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "a string' with a quote")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 26)
  }

  func testStringWithSeveralQuotes() throws {
    let scanner = Scanner(on: "'a string'' with ''''several quotes''' other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .string)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "a string' with ''several quotes'")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 39)
  }

  func testSymbol() throws {
    let scanner = Scanner(on: "#aSymbol other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .symbol)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "#aSymbol")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 9)
  }

  func testSymbolStartingWithNumber() throws {
    let scanner = Scanner(on: "#123aSymbol other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .error)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "symbol must start with a letter unless using symbol string syntax")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 0)
  }

  func testBinarySymbol() throws {
    let binaryChars = ["-", ",", "+", "/", "\\", "*", "~", "<", ">", "=", "@", "%", "|", "&", "?", "!"]
    var binarySelectors: [String] = []
    for char1 in binaryChars {
      binarySelectors.append(char1)
      for char2 in binaryChars {
        binarySelectors.append(char1+char2)
      }
    }
    for selector in binarySelectors {
      let selectorString = "#" + selector + " other stuff"
      let scanner = Scanner(on: selectorString)
      let token = scanner.nextToken()
      XCTAssertEqual(token.type, .symbol)
      if let valueToken = token as? ValueToken {
        XCTAssertEqual(valueToken.value, "#"+selector)
      } else {
        XCTFail("Expected ValueToken!")
      }
      XCTAssertEqual(token.position, 0)
      XCTAssertEqual(scanner.position, selector.count+2)
    }
  }

  func testKeywordSymbol() throws {
    let scanner = Scanner(on: "#aSymbol: other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .symbol)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "#aSymbol:")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 10)
  }

  func testStringSymbol() throws {
    let scanner = Scanner(on: "#'12true' other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .symbol)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "#12true")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 10)
  }

  func testSimpleInteger() throws {
    let scanner = Scanner(on: "123 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "123")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 4)
  }

  func testSimpleIntegerWithRadix() throws {
    let scanner = Scanner(on: "8r123 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "8r123")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 6)
  }

  func testNegativeInteger() throws {
    let scanner = Scanner(on: "-123 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "-123")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 5)
  }

  func testNegativeIntegerWithRadix() throws {
    let scanner = Scanner(on: "8r-123 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "8r-123")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 7)
  }

  func testSimpleFloat() throws {
    let scanner = Scanner(on: "123.45 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "123.45")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 7)
  }

  func testSimpleIntegerWithPositiveExponent() throws {
    let scanner = Scanner(on: "123e3 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "123e3")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 6)
  }

  func testSimpleIntegerWithNegativeExponent() throws {
    let scanner = Scanner(on: "123e-3 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "123e-3")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 7)
  }

  func testSimpleFloatWithPositiveExponent() throws {
    let scanner = Scanner(on: "123.45e3 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "123.45e3")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 9)
  }

  func testSimpleFloatWithNegativeExponent() throws {
    let scanner = Scanner(on: "123.45e-3 other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .number)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "123.45e-3")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 10)
  }

  func testCharacter() throws {
    let scanner = Scanner(on: "$d other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .character)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "d")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 3)
  }

  func testSpaceCharacter() throws {
    let scanner = Scanner(on: "$  other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .character)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, " ")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 3)
  }

  func testCharacterOpenSquareBracket() throws {
    let scanner = Scanner(on: "$[ other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .character)
    if let valueToken = token as? ValueToken {
      XCTAssertEqual(valueToken.value, "[")
    } else {
      XCTFail("Expected ValueToken!")
    }
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 3)
  }

  func testLitteralArray() throws {
    let scanner = Scanner(on: "#(1 $n (3 4) 'stuff') other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .openArray)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 2)
  }

  func testLitteralByteArray() throws {
    let scanner = Scanner(on: "#[1 34 72 15] other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .openByteArray)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 2)
  }

  func testReturn() throws {
    let scanner = Scanner(on: "^self other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .caret)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 1)
  }

  func testOpenParens() throws {
    let scanner = Scanner(on: "(1 + 3)")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .openParen)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 1)
  }

  func testCloseParens() throws {
    let scanner = Scanner(on: ") other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .closeParen)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 2)
  }

  func testOpenSquareParens() throws {
    let scanner = Scanner(on: "[other stuff]")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .openSquareParen)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 1)
  }

  func testCloseSquareParens() throws {
    let scanner = Scanner(on: "]. ^self other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .closeSquareParen)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 1)
  }

  func testPeriod() throws {
    let scanner = Scanner(on: ". ^self other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .period)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 2)
  }

  func testOpenCurlyParens() throws {
    let scanner = Scanner(on: "{ self . other . stuff }")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .openCurlyParen)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 2)
  }

  func testCloseCurlyParens() throws {
    let scanner = Scanner(on: "}. self other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .closeCurlyParen)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 1)
  }

  func testSemiColon() throws {
    let scanner = Scanner(on: "; other stuff")
    let token = scanner.nextToken()
    XCTAssertEqual(token.type, .semiColon)
    XCTAssertEqual(token.position, 0)
    XCTAssertEqual(scanner.position, 2)
  }
}
