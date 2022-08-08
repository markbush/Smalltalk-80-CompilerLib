public protocol VisitableNode {
  func accept(_ visitor: NodeVisitor)
}
