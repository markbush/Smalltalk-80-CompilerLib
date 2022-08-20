public class LiteralArrayNode : LiteralNode {
  var values: [LiteralNode] = []

  override public var description: String {
    "(LiteralArrayNode value: \(values))"
  }
  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralArrayNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    context.saveLiteralArray("\(values)")
  }
}
