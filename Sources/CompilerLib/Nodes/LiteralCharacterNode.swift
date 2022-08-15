public class LiteralCharacterNode : LiteralNode {
  let value: String

  public override var description: String {
    "(LiteralCharacterNode value: \(value))"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralCharacterNode(self)
  }
}
