public class AssignNode : ExpressionNode {
  var variable: VariableNode? = nil
  var value: ExpressionNode? = nil

  public override var description: String {
    var parts = ["(AssignNode"]
    if let variableNode = variable {
      parts.append("variable: \(variableNode)")
    }
    if let valueNode = value {
      parts.append("\n        value: \(valueNode)")
    }
    return "\(parts.joined(separator: " ")))"
  }

}
