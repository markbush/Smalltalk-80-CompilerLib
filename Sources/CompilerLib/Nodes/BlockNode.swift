public class BlockNode : CodeNode {

  public override var description: String {
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
}
