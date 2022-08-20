public class MessageNode : ExpressionNode {
  let receiver: ExpressionNode
  let selector: String
  var arguments: [ExpressionNode] = []

  var numArguments: Int {
    arguments.count
  }

  override public var description: String {
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

  override public var sendsToSuper: Bool {
    if let superSend = receiver as? VariableNode, superSend.name == "super" {
      return true
    } else {
      return false
    }
  }

  public init(receiver: ExpressionNode, selector: String) {
    self.receiver = receiver
    self.selector = selector
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitMessageNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    if context.specialSelectors.contains(selector) {
      for argument in arguments.reversed() {
        argument.addLiteralsTo(context)
      }
      receiver.addLiteralsTo(context)
    } else {
      context.saveSelectorFor(self)
      receiver.addLiteralsTo(context)
      for argument in arguments {
        argument.addLiteralsTo(context)
      }
    }
  }
  override public func returns() -> Bool {
    switch selector {
    case "ifTrue:ifFalse:", "ifFalse:ifTrue:":
      return arguments[0].returns() && arguments[1].returns()
    default: return false
    }
  }
}
