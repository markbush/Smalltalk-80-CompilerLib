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
  override public func addLiteralsTo(_ context: CompilerContext) {
    if !context.variableIsKnown(name) {
      if name.first?.isUppercase ?? false {
        let _ = context.indexForLiteralVariable(name)
      } else {
        context.saveLiteralSymbol(name)
      }
    }
  }
  override public func returns() -> Bool {
    return false
  }
}
