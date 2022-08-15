public class LiteralNumberNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralNumberNode value: \(value))"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralNumberNode(self)
  }
}
