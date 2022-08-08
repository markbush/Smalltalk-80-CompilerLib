public class DynamicArrayNode : ExpressionNode {
  var values: [StatementNode] = []

  public override var description: String {
    var parts = ["(DynamicArrayNode:",
      "  values: ["
    ]
    for value in values {
      parts.append("\(value)")
    }
    return String(parts.joined(separator: "\n"))+"])"
  }
}
