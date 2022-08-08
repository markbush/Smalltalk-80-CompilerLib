public class Node : CustomStringConvertible, VisitableNode {
  var comments: [String] = []

  public var description: String {
    "(Node)"
  }

  public func accept(_ visitor: NodeVisitor) {
    fatalError("\"accept()\" must be implemented in class \(String(describing: type(of: self)))")
  }
}
