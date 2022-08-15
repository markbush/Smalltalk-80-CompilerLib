public class LiteralStringNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralStringNode value: '\(value)')"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralStringNode(self)
  }
}
