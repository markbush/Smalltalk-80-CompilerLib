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
  override public func addLiteralsTo(_ context: CompilerContext) {
    if let codeBody = body {
      for statement in codeBody.statements {
        statement.addLiteralsTo(context)
      }
    }
  }
  override public func returns() -> Bool {
    if let bodyNode = body {
      return bodyNode.returns()
    }
    return false
  }
}
