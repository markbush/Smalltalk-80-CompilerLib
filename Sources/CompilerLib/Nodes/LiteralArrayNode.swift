public class LiteralArrayNode : LiteralNode {
  var values: [LiteralNode] = []

  override public var description: String {
    "(LiteralArrayNode value: \(values))"
  }
}
