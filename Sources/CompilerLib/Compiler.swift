import Foundation

public class Compiler : NodeVisitor, CustomStringConvertible {
  let context: CompilerContext
  var source = ""
  var hasReturned = false

  public var description: String {
    let parts = [source,
                 "---------------------------",
                 "\(context)",
                 ""
               ]
    return parts.joined(separator: "\n")
  }

  public init(forClass classDescription: ClassDescription) {
    self.context = CompilerContext(forClass: classDescription)
  }
  public init() {
    self.context = CompilerContext(forClass: ClassDescription("Test", instanceVariables: []))
  }

  public func compileMethod(_ method: String) -> String {
    var output: [String] = []
    source = method.replacingOccurrences(of: "\t", with: "  ")
    let parser = Parser(on: source)
    do {
      let ast = try parser.parseMethod()
      output.append(source)
      output.append("\(ast)")
      context.reset()
      ast.accept(self)
    } catch ParserError.syntaxError(let reason, let position) {
      output.append(reason)
      if let charPos = position {
        let contentLines = source.components(separatedBy: "\n")
        var errorLocation = charPos
        for line in contentLines {
          output.append(line)
          if errorLocation >= 0 && errorLocation < line.count {
            output.append(String(repeating: " ", count: errorLocation)+"^")
            break
          }
          errorLocation -= line.count + 1
        }
      }
    } catch {
      print(error)
    }
    return output.joined(separator: "\n")
  }

  public func visitMethodNode(_ node: MethodNode) {
    context.selector = node.selector
    for variableNode in node.arguments {
      context.addArg(variableNode.name)
    }
    if let body = node.body {
      body.accept(self)
    }
    if !hasReturned {
      context.push(.returnSelf)
    }
  }

  public func visitStatementListNode(_ node: StatementListNode) {
    for variableNode in node.temporaries {
      context.addTemp(variableNode.name)
    }
    // TODO: Pragmas
    for statement in node.statements {
      statement.accept(self)
    }
  }

  public func visitReturnNode(_ node: ReturnNode) {
    node.value.accept(self)
    context.push(.returnTop)
    hasReturned = true
  }

  public func visitMessageNode(_ node: MessageNode) {
    context.saveSelectorFor(node)
    node.receiver.accept(self)
    for argument in node.arguments {
      argument.accept(self)
    }
    context.pushSelectorFor(node)
  }

  public func visitVariableNode(_ node: VariableNode) {
    context.pushVariable(node.name)
  }

  func isInteger(_ number: String) -> Bool {
    if number.count == 0 {
      return false
    }
    var rest = number
    if let first = number.first, first == "-" {
      rest = String(number.dropFirst())
    }
    for c in rest {
      if !c.isNumber {
        return false
      }
    }
    return true
  }

  public func visitLiteralNumberNode(_ node: LiteralNumberNode) {
    let value = node.value
    if isInteger(value) {
      if let number = Int(value), number >= -1 && number <= 2 {
        guard let bytecode = Bytecode(rawValue: Bytecode.pushZero.rawValue + number) else {
          fatalError("Bytecodes 116-119 (push -1, 0, 1, 2) not set up correctly!")
        }
        context.push(bytecode)
      }
    }
  }

  public func visitAssignNode(_ node: AssignNode) {
    guard let variableNode = node.variable, let value = node.value else {
      fatalError("Invalid assign node \(node)")
    }
    value.accept(self)
    context.popVariable(variableNode.name)
  }
}
