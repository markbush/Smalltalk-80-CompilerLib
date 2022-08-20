public class CascadeMessageNode : ExpressionNode {
  var messages: [ExpressionNode] = []

  override public var description: String {
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
  override public func addLiteralsTo(_ context: CompilerContext) {
    if messages.count > 0 {
      guard let firstMessage = messages[0] as? MessageNode else {
        fatalError("Cascades should only contain messages")
      }
      firstMessage.receiver.addLiteralsTo(context)
    }
    for message in messages {
      message.addLiteralsTo(context)
    }
  }
  override public func returns() -> Bool {
    for message in messages {
      if message.returns() {
        return true
      }
    }
    return false
  }
}
