public class Scanner {
  let source: String
  let inputStream: [Character]
  var position = -1
  var tokenStart = 0
  var currentCharacter: Character? = nil
  var characterType: CharacterType = .null
  var lastToken = Token(.eof, at: 0)
  var inArray = 0
  var inByteArray = false
  public private(set) var comments: [String] = []

  public init(on source: String) {
    self.source = source
    self.inputStream = Array(source)
    initialise()
  }

  func initialise() {
    step()
    skipIgnores()
  }

  public func nextToken() -> Token {
    let token = next()
    skipIgnores()
    token.comments.append(contentsOf: comments)
    comments = []
    lastToken = token
    return token
  }

  func next() -> Token {
    if characterType == .eof {
      return Token(.eof, at: tokenStart)
    }

    switch characterType {
    case .letter: return scanIdentifierOrKeyword()
    case .digit: return scanNumber()
    case .colon, .underscore: return scanAssignOrBlockArg()
    case .binary: return scanNegativeOrBinarySelector()
    case .string: return scanString()
    case .hash: return scanSymbolOrArrayLitteral()
    case .dollar: return scanCharacter()
    case .special, .period: return scanSpecial()
    default: return ValueToken(.unknown, at: position, with: String(currentCharacter!))
    }
  }

  @discardableResult func step() -> Character? {
    position += 1
    if position >= inputStream.count {
      currentCharacter = nil
      characterType = .eof
    } else {
      currentCharacter = inputStream[position]
      characterType = classify(currentCharacter!)
    }
    return currentCharacter
  }

  func peek() -> Character? {
    if position+1 >= inputStream.count {
      return nil
    } else {
      return inputStream[position+1]
    }
  }

  func classify(_ char: Character) -> CharacterType {
    switch char {
    case "+", "/", "\\", "*", "~", "<", ">", "=", "@", "%", "|", "&", "?", "!", ",", "-": return .binary
    case "a"..."z", "A"..."Z": return .letter
    case "0"..."9": return .digit
    case ":": return .colon
    case "_": return .underscore
    case "'": return .string
    case "\"": return .comment
    case "#": return .hash
    case ".": return .period
    case "$": return .dollar
    default:
      if char.isWhitespace {
        return .whitespace
      }
      return .special
    }
  }

  func skipIgnores() {
    skipWhitespace()
    while characterType == .comment {
      scanComment()
      skipWhitespace()
    }
  }

  func skipWhitespace() {
    while characterType == .whitespace {
      step()
    }
  }

  func scanComment() {
    var comment = ""
    while step() != nil {
      if characterType == .comment {
        step()
        if characterType != .comment {
          comments.append(comment)
          return
        }
        comment.append(currentCharacter!)
      } else {
        comment.append(currentCharacter!)
      }
    }
  }

  func scanIdentifierOrKeyword() -> Token {
    tokenStart = position
    var value = ""
    value.append(currentCharacter!)

    while step() != nil {
      switch characterType {
      case .letter, .digit:
        value.append(currentCharacter!)
      case .colon:
        value.append(currentCharacter!)
        if inArray == 0 {
          step()
          return ValueToken(.keyword, at: tokenStart, with: value)
        }
      default:
        return ValueToken(.identifier, at: tokenStart, with: value)
      }
    }
    return ValueToken(.identifier, at: tokenStart, with: value)
  }

  func scanAssignOrBlockArg() -> Token {
    tokenStart = position
    if characterType == .underscore {
      step()
      return Token(.assign, at: tokenStart)
    }
    if let nextChar = peek() , nextChar == "=" {
      step()
      step()
      return Token(.assign, at: tokenStart)
    }
    // Allow spaces between colon and arg name
    while peek() != nil && classify(peek()!) == .whitespace {
      step()
    }
    var value = ""
    loop: while step() != nil {
      switch characterType {
      case .letter:
        value.append(currentCharacter!)
      case .digit where position > tokenStart+1:
        value.append(currentCharacter!)
      case .digit:
        return ValueToken(.error, at: tokenStart, with: "an identifier must start with a letter")
      default:
        break loop
      }
    }
    if value.count > 0 {
      return ValueToken(.blockArg, at: tokenStart, with: value)
    }
    return ValueToken(.error, at: tokenStart, with: "unexpected ':'")
  }

  func scanBinarySelector() -> Token {
    tokenStart = position
    let value = scanBinarySequence()
    return ValueToken(.binary, at: tokenStart, with: value)
  }

  func scanBinarySymbol() -> Token {
    step()
    let value = "#"+scanBinarySequence()
    return ValueToken(.symbol, at: tokenStart, with: value)
  }

  func scanBinarySequence() -> String {
    var value = ""
    value.append(currentCharacter!)
    if let nextChar = peek(), classify(nextChar) == .binary {
      step()
      value.append(currentCharacter!)
      if let nextChar = peek(), classify(nextChar) == .binary {
        step()
        value.append(currentCharacter!)
      }
    }
    step()
    return value
  }

  func scanString() -> Token {
    tokenStart = position
    var value = ""
    while step() != nil {
      if characterType == .string {
        step()
        if characterType != .string {
          return ValueToken(.string, at: tokenStart, with: value)
        }
        value.append(currentCharacter!)
      } else {
        value.append(currentCharacter!)
      }
    }
    return ValueToken(.error, at: tokenStart, with: "unmatched \"'\"")
  }

  func scanSymbolOrArrayLitteral() -> Token {
    tokenStart = position
    while peek() != nil && peek() == "#" {
      step()
    }
    guard let nextChar = peek() else {
      return ValueToken(.error, at: tokenStart, with: "eof while reading symbol")
    }
    switch classify(nextChar) {
    case .special where nextChar == "(":
      step()
      step()
      inArray += 1
      return Token(.openArray, at: tokenStart)
    case .special where nextChar == "[":
      step()
      step()
      inByteArray = true
      return Token(.openByteArray, at: tokenStart)
    case .binary: return scanBinarySymbol()
    case .letter: return scanSymbol()
    case .string: return scanStringSymbol()
    case .digit:
      return ValueToken(.error, at: tokenStart, with: "symbol must start with a letter unless using symbol string syntax")
    default: return ValueToken(.error, at: tokenStart, with: "empty symbol")
    }
  }

  func scanSymbol() -> Token {
    var value = "#"
    loop: while step() != nil {
      switch characterType {
      case .letter, .digit, .colon:
        value.append(currentCharacter!)
      default:
        break loop
      }
    }
    if (value.count > 0) {
    return ValueToken(.symbol, at: tokenStart, with: value)
    }
    return ValueToken(.error, at: tokenStart, with: "empty symbol")
  }

  func scanStringSymbol() -> Token {
    var value = "#"
    step()
    while step() != nil {
      switch characterType {
      case .string:
        step()
      return ValueToken(.symbol, at: tokenStart, with: value)
      default:
        value.append(currentCharacter!)
      }
    }
    return ValueToken(.error, at: tokenStart, with: "unmatched \"'\"")
  }

  func scanNegativeOrBinarySelector() -> Token {
    if let char = currentCharacter, char == "-" {
      if let nextChar = peek(), classify(nextChar) == .digit {
        if inArray > 0 || inByteArray {
          return scanNumber()
        }
        switch lastToken.type {
        case .identifier, .string, .symbol, .number, .character, .closeParen, .closeSquareParen, .closeCurlyParen, .semiColon:
          return scanBinarySelector()
        default:
          return scanNumber()
        }
      }
    }
    return scanBinarySelector()
  }

  func scanNumber() -> Token {
    tokenStart = position
    var value = ""
    // currentChar is a digit
    value.append(scanDigitSequence())
    if characterType == .letter && (currentCharacter! == "r" || currentCharacter! == "R") {
      guard let radixValue = Int(value) else {
        return ValueToken(.error, at: tokenStart, with: "invalid radix")
      }
      value.append("r")
      step()
      if currentCharacter == nil {
        return ValueToken(.error, at: tokenStart, with: "missing number after radix")
      }
      if radixValue <= 10 && characterType != .digit && currentCharacter! != "-" {
        return ValueToken(.error, at: tokenStart, with: "non-digit after radix")
      }
      if characterType != .digit && characterType != .letter && currentCharacter! != "-" {
        return ValueToken(.error, at: tokenStart, with: "malformed number")
      }
      value.append(scanDigitSequence(withRadix: radixValue))
    }
    if characterType == .period {
      if let nextChar = peek(), classify(nextChar) != .digit {
        return ValueToken(.number, at: tokenStart, with: value)
      }
      value.append(currentCharacter!)
      step()
      if characterType == .digit {
        value.append(scanDigitSequence())
      }
    }
    if characterType == .letter && (currentCharacter! == "e" || currentCharacter! == "E") {
      value.append(currentCharacter!)
      step()
      if characterType == .binary && currentCharacter! == "-" {
        value.append(currentCharacter!)
        step()
      }
      if characterType == .digit {
        value.append(scanDigitSequence())
      } else {
        return ValueToken(.error, at: tokenStart, with: "missing exponent")
      }
    }
    return ValueToken(.number, at: tokenStart, with: value)
  }

  func scanDigitSequence(withRadix radix: Int = 10) -> String {
    var value = ""
    if let minus = currentCharacter, minus == "-" {
      value = "-"
      step()
    }
    if currentCharacter == nil {
      return value
    }
    if radix <= 10 && characterType != .digit {
      return value
    }
    if characterType != .letter && characterType != .digit {
      return value
    }
    value.append(currentCharacter!)
    loop: while step() != nil {
      if characterType == .digit {
        value.append(currentCharacter!)
      } else {
        if radix > 10 && characterType == .letter {
          value.append(currentCharacter!)
        } else {
          break loop
        }
      }
    }
    return value
  }

  func scanCharacter() -> Token {
    tokenStart = position
    step()
    switch characterType {
    case .eof:
      return ValueToken(.error, at: tokenStart, with: "end while reading character constant")
    case .letter, .digit, .binary, .colon, .string, .comment, .hash, .period, .dollar, .special, .underscore:
      let value = String(currentCharacter!)
      step()
      return ValueToken(.character, at: tokenStart, with: value)
    case .whitespace where currentCharacter! == " ":
      let value = String(currentCharacter!)
      step()
      return ValueToken(.character, at: tokenStart, with: value)
    default:
      return ValueToken(.error, at: tokenStart, with: "unknown character constant format: $\(currentCharacter!)")
    }
  }

  func scanSpecial() -> Token {
    tokenStart = position
    switch currentCharacter! {
    case "^":
      step()
      return Token(.caret, at: tokenStart)
    case "[":
      step()
      return Token(.openSquareParen, at: tokenStart)
    case "]":
      step()
      if inByteArray {
        inByteArray = false
      }
      return Token(.closeSquareParen, at: tokenStart)
    case "(":
      step()
      if inArray > 0 {
        inArray += 1
      }
      return Token(.openParen, at: tokenStart)
    case ")":
      step()
      if inArray > 0 {
        inArray -= 1
      }
      return Token(.closeParen, at: tokenStart)
    case "{":
      step()
      return Token(.openCurlyParen, at: tokenStart)
    case "}":
      step()
      return Token(.closeCurlyParen, at: tokenStart)
    case ".":
      step()
      return Token(.period, at: tokenStart)
    case ";":
      step()
      return Token(.semiColon, at: tokenStart)
    default:
      return ValueToken(.unknown, at: tokenStart, with: String(currentCharacter!))
    }
  }
}
