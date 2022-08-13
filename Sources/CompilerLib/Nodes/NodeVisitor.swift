public protocol NodeVisitor {
  func visitMethodNode(_ node: MethodNode)
  func visitStatementListNode(_ node: StatementListNode)
  func visitReturnNode(_ node: ReturnNode)
  func visitMessageNode(_ node: MessageNode)
  func visitVariableNode(_ node: VariableNode)
  func visitLiteralNumberNode(_ node: LiteralNumberNode)
  func visitAssignNode(_ node: AssignNode)
  func visitLiteralStringNode(_ node: LiteralStringNode)
  func visitCascadeMessageNode(_ node: CascadeMessageNode)
  func visitLiteralCharacterNode(_ node: LiteralCharacterNode)
}
