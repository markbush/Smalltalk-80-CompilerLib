public class LiteralStringNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralStringNode value: '\(value)')"
  }

  init(_ value: String) {
    self.value = value
  }
}
