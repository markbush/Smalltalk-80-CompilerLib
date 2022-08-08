public class LiteralSymbolNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralSymbolNode value: \(value))"
  }

  init(_ value: String) {
    self.value = value
  }
}
