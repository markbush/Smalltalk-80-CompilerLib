public class VariableNode : ExpressionNode {
  let name: String

  public init(_ name: String) {
    self.name = name
  }

  public override var description: String {
    "(VariableNode name: \(name))"
  }
}
