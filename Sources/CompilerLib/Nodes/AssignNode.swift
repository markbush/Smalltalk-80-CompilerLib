public class AssignNode : ExpressionNode {
  var variable: VariableNode? = nil
  var value: ExpressionNode? = nil

  override public var description: String {
    var parts = ["(AssignNode"]
    if let variableNode = variable {
      parts.append("variable: \(variableNode)")
    }
    if let valueNode = value {
      parts.append("\n        value: \(valueNode)")
    }
    return "\(parts.joined(separator: " ")))"
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitAssignNode(self)
  }
}
