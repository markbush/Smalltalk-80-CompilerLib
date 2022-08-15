public class ReturnNode : StatementNode {
  let value: ExpressionNode

  public override var description: String {
    "(ReturnNode value: \(value))"
  }

  public init(_ expression: ExpressionNode) {
    self.value = expression
    super.init()
    self.leavesValueOnStack = false
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitReturnNode(self)
  }
}
