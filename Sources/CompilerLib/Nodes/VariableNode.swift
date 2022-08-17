public class VariableNode : ExpressionNode {
  let name: String

  public init(_ name: String) {
    self.name = name
  }

  override public var description: String {
    "(VariableNode name: \(name))"
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitVariableNode(self)
  }
}
