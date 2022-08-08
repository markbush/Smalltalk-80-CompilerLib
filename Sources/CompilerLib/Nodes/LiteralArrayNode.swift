public class LiteralArrayNode : LiteralNode {
  var values: [LiteralNode] = []

  public override var description: String {
    "(LiteralArrayNode value: \(values))"
  }
}
