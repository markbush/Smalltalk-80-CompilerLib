public class Node : CustomStringConvertible, VisitableNode {
  weak var parent: Node? = nil
  var comments: [String] = []

  public var description: String {
    "(Node)"
  }

  public var parentIsAssign: Bool {
    switch parent {
    case is AssignNode: return true
    default: return false
    }
  }

  public func accept(_ visitor: NodeVisitor) {
    fatalError("\"accept()\" must be implemented in class \(String(describing: type(of: self)))")
  }
}
