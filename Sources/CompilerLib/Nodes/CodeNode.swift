public class CodeNode : ExpressionNode {
  var arguments: [VariableNode] = []
  var body: StatementListNode? = nil

  override public var description: String {
    var parts = ["CodeNode:",
    "  Arguments: \(arguments)"]
    if comments.count > 0 {
      parts.append("Comments: \"\(comments.joined(separator: "\n"))\"")
    }
    if let statements = body {
      parts.append("  Body: \(statements)")
    }
    return "("+String(parts.joined(separator: "\n"))+")"
  }
}
