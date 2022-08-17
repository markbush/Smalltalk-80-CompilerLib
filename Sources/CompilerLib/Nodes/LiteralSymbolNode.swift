public class LiteralSymbolNode : LiteralNode {
  let value: String

  override public var description: String {
    "(LiteralSymbolNode value: \(value))"
  }

  public init(_ value: String) {
    self.value = value
  }
}
