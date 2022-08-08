public class LiteralCharacterNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralCharacterNode value: \(value))"
  }

  init(_ value: String) {
    self.value = value
  }
}
