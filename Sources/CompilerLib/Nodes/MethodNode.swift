public class MethodNode : CodeNode {
  let selector: String

  public override var description: String {
    var parts = ["MethodNode:",
    "  Selector: #\(selector)",
    "  Arguments: \(arguments)"]
    if comments.count > 0 {
      parts.append("Comments: \"\(comments.joined(separator: "\n"))\"")
    }
    if let statements = body {
      parts.append("  Body: \(statements)")
    }
    return String(parts.joined(separator: "\n"))
  }

  public init(_ selector: String) {
    self.selector = selector
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitMethodNode(self)
  }
}
