public protocol NodeVisitor {
  func visitMethodNode(_ node: MethodNode)
  func visitStatementListNode(_ node: StatementListNode)
  func visitReturnNode(_ node: ReturnNode)
  func visitMessageNode(_ node: MessageNode)
}
