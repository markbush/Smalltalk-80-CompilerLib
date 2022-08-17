public class LiteralByteArrayNode : LiteralNode {
  var values: [LiteralNode] = []

  override public var description: String {
    "(LiteralByteArrayNode value: \(values))"
  }
}
