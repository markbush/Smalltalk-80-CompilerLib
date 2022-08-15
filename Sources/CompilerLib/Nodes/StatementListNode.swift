public class StatementListNode : Node {
  var statements: [StatementNode] = []
  var temporaries: [VariableNode] = []
  var pragmas: [PragmaNode] = []
  var inLoop = false

  public override var description: String {
    var parts = ["(StatementListNode:"]
    if temporaries.count > 0 {
      parts.append("  Temporaries: \(temporaries)")
    }
    if pragmas.count > 0 {
      parts.append("  Pragmas: \(pragmas)")
    }
    parts.append("  Statements: ")
    for statement in statements {
      parts.append("\(statement)")
    }
    return String(parts.joined(separator: "\n"))+")"
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitStatementListNode(self)
  }
}
