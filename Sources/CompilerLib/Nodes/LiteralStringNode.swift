public class LiteralStringNode : LiteralNode {
  let value: String

  override public var description: String {
    "(LiteralStringNode value: '\(value)')"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralStringNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    context.saveLiteralString(value)
  }
}
