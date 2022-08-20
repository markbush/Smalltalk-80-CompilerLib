public class BlockNode : CodeNode {

  override public var description: String {
    var parts = ["BlockNode:",
    "  Arguments: \(arguments)"]
    if comments.count > 0 {
      parts.append("Comments: \"\(comments.joined(separator: "\n"))\"")
    }
    if let statements = body {
      parts.append("  Body: \(statements)")
    }
    return "("+String(parts.joined(separator: "\n"))+")"
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitBlockNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    for argument in arguments {
      context.saveTempVar(argument.name)
    }
    super.addLiteralsTo(context)
  }
}
