public class LiteralByteArrayNode : LiteralNode {
  var values: [LiteralNode] = []

  public override var description: String {
    "(LiteralByteArrayNode value: \(values))"
  }
}
