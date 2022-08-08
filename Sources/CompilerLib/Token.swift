public class Token : CustomStringConvertible {
  public let type: TokenType
  public let position: Int
  public var comments: [String] = []

  public var description: String {
    "\(type)(at: \(position))"
  }

  public init(_ type: TokenType, at position: Int) {
    self.type = type
    self.position = position
  }
}
