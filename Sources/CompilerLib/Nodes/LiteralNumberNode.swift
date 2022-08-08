public class LiteralNumberNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralNumberNode value: \(value))"
  }

  init(_ value: String) {
    self.value = value
  }
}
