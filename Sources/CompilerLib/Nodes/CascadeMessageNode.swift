public class CascadeMessageNode : ExpressionNode {
  var messages: [ExpressionNode] = []

  public override var description: String {
    var parts = ["(CascadeMessageNode:",
      "  Messages: ["
    ]
    for message in messages {
      parts.append("\(message)")
    }
    return String(parts.joined(separator: "\n"))+"])"
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitCascadeMessageNode(self)
  }
}
