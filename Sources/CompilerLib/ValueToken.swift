public class ValueToken : Token {
  public let value: String

  public override var description: String {
    "\(type)(\"\(value)\", at: \(position))"
  }

  public init(_ type: TokenType, at position: Int, with value: String) {
    self.value = value
    super.init(type, at: position)
  }
}
