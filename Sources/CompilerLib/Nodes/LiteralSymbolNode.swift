public class LiteralSymbolNode : LiteralNode {
  let value: String

  override public var description: String {
    "(LiteralSymbolNode value: \(value))"
  }

  public init(_ value: String) {
    self.value = value
  }

  override public func accept(_ visitor: NodeVisitor) {
    visitor.visitLiteralSymbolNode(self)
  }
  override public func addLiteralsTo(_ context: CompilerContext) {
    context.saveLiteralSymbol(value)
  }
}
