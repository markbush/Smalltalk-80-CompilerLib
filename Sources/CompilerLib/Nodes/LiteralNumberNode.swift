public class LiteralNumberNode : LiteralNode {
  let value: String

  override public var description: String {
    "(LiteralNumberNode value: \(value))"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralNumberNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    switch value {
    case "-1", "0", "1", "2": break
    default: context.saveInteger(value)
    }
  }
  override public func returns() -> Bool {
    return false
  }
}
