public class MessageNode : ExpressionNode {
  let receiver: ExpressionNode
  let selector: String
  var arguments: [ExpressionNode] = []

  var numArguments: Int {
    arguments.count
  }

  public override var description: String {
    var result = "(MessageNode receiver: \(receiver) selector: \(selector)"
    if arguments.count > 0 {
      result.append("\n  arguments: [")
      for argument in arguments {
        result.append("\(argument)\n        ")
      }
      result.append("]")
    }
    result.append(")")
    return result
  }

  init(receiver: ExpressionNode, selector: String) {
    self.receiver = receiver
    self.selector = selector
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitMessageNode(self)
  }
}
