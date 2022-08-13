import Foundation

public class Compiler : NodeVisitor, CustomStringConvertible {
  var context: CompilerContext
  var source = ""
  let specialSelectors = [
    "ifTrue:", "ifFalse:", "ifTrue:ifFalse:", "ifFalse:ifTrue:",
			"and:", "or:", "whileFalse:", "whileTrue:"
  ]

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
      if (!context.returns()) {
        context.push(.returnSelf)
      }
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
  }

  public func visitStatementListNode(_ node: StatementListNode) {
    for variableNode in node.temporaries {
      context.addTemp(variableNode.name)
    }
    // TODO: Pragmas
    for i in 0..<node.statements.count {
      let statement = node.statements[i]
      statement.accept(self)
      // If the statement has left a value on the stack, it needs to be popped
      switch statement {
      case is AssignNode: break
      case is ReturnNode: break
      default:
        if node.parent is BlockNode && i == node.statements.count - 1 {
          // Block returns last statement
          break
        }
        if i == node.statements.count - 1 && context.returns() {
          // return so don't pop
          break
        }
        // Ignore result of last expression
        context.push(.popStack)
      }
    }
  }

  public func visitReturnNode(_ node: ReturnNode) {
    node.value.accept(self)
    context.push(.returnTop)
  }

  func saveContext() -> CompilerContext {
    let savedContext = context
    context = CompilerContext(forClass: savedContext.classDescription)
    context.arguments = savedContext.arguments
    context.temporaries = savedContext.temporaries
    context.literals = savedContext.literals
    return savedContext
  }

  func unwindContextWith(_ bytecode: Bytecode) {
    context.bytecodes[context.bytecodes.count-2] = bytecode
    let _ = context.bytecodes.popLast()
  }

  func restoreContextFrom(_ savedContext: CompilerContext) {
    if context.bytecodes.count >= 2 && context.bytecodes[context.bytecodes.count-1] == .returnTop {
      let prevBytecode = context.bytecodes[context.bytecodes.count-2]
      switch prevBytecode {
      case .pushSelf: unwindContextWith(.returnSelf)
      case .pushTrue: unwindContextWith(.returnTrue)
      case .pushFalse: unwindContextWith(.returnFalse)
      case .pushNil: unwindContextWith(.returnNil)
      default: break
      }
    }
    savedContext.bytecodes.append(contentsOf: context.bytecodes)
    savedContext.literals = context.literals
    context = savedContext
  }

  func handleSpecialSelector(_ node: MessageNode) {
    switch node.selector {
    case "ifTrue:ifFalse:": handleIfTrueIfFalse(node)
    case "and:": handleAnd(node)
    default: fatalError("No implementation for \(node.selector)")
    }
  }

  func handleAnd(_ node: MessageNode) {
    guard node.arguments.count == 1 else {
      fatalError("Wrong number of arguments for #and: \(node.arguments.count) (expected 1)")
    }
    guard let argNode = node.arguments[0] as? BlockNode else {
      fatalError("Arguments to #ifTrue:ifFalse: must be blocks")
    }
    guard let argBody = argNode.body else {
      return
    }
    let savedContext = saveContext()
    argBody.accept(self)
    context.push(.jump1)
    context.push(.pushFalse)
    let numArgBytes = context.bytecodes.count
    savedContext.pushConditionalJumpOn(false, numBytes: numArgBytes)
    restoreContextFrom(savedContext)
  }

  func handleIfTrueIfFalse(_ node: MessageNode) {
    guard node.arguments.count == 2 else {
      fatalError("Wrong number of arguments for #ifTrue:ifFalse: \(node.arguments.count) (expected 2)")
    }
    guard let trueNode = node.arguments[0] as? BlockNode else {
      fatalError("Arguments to #ifTrue:ifFalse: must be blocks")
    }
    guard let falseNode = node.arguments[1] as? BlockNode else {
      fatalError("Arguments to #ifTrue:ifFalse: must be blocks")
    }
    guard let trueBody = trueNode.body else {
      return
    }
    guard let falseBody = falseNode.body else {
      return
    }

    let savedContext = saveContext()
    trueBody.accept(self)
    let numTrueBytes = context.bytecodes.count
    savedContext.pushConditionalJumpOn(false, numBytes: numTrueBytes)
    falseBody.accept(self)
    restoreContextFrom(savedContext)
  }

  func handleMessageSendFor(_ node: MessageNode) {
    context.saveSelectorFor(node)
    for argument in node.arguments {
      argument.accept(self)
    }
    context.pushSelectorFor(node)
  }

  public func visitMessageNode(_ node: MessageNode) {
    if specialSelectors.contains(node.selector) {
      node.receiver.accept(self)
      handleSpecialSelector(node)
      return
    }
    node.receiver.accept(self)
    handleMessageSendFor(node)
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
        return
      }
      if let number = Int(value), number >= -16384 && number <= 16383 {
        context.pushSmallInteger(number)
        return
      }
    }
    fatalError("Cannot handle numbers")
  }

  public func visitAssignNode(_ node: AssignNode) {
    guard let variableNode = node.variable, let value = node.value else {
      fatalError("Invalid assign node \(node)")
    }
    value.accept(self)
    if node.parentIsBody {
      context.popVariable(variableNode.name)
    } else {
      context.storeVariable(variableNode.name)
    }
  }

  public func visitLiteralStringNode(_ node: LiteralStringNode) {
    let value = node.value
    context.pushLiteralString(value)
  }

  public func visitLiteralCharacterNode(_ node: LiteralCharacterNode) {
    let value = node.value
    context.pushLiteralCharacter(value)
  }

  public func visitCascadeMessageNode(_ node: CascadeMessageNode) {
    if node.messages.count == 0 {
      return
    }
    guard let firstMessage = node.messages[0] as? MessageNode else {
      fatalError("Cascades should only contain messages")
    }
    firstMessage.receiver.accept(self)
    for i in 0 ..< node.messages.count - 1 {
      context.push(.dupTop)
      guard let message = node.messages[i] as? MessageNode else {
        fatalError("Cascades should only contain messages")
      }
      handleMessageSendFor(message)
      context.push(.popStack)
    }
    guard let lastMessage = node.messages[node.messages.count-1] as? MessageNode else {
      fatalError("Cascades should only contain messages")
    }
    handleMessageSendFor(lastMessage)
  }
}
