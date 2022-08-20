public class LiteralCharacterNode : LiteralNode {
  let value: String

  override public var description: String {
    "(LiteralCharacterNode value: \(value))"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralCharacterNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    context.saveLiteralCharacter(value)
  }
}
