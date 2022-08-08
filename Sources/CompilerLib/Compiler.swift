import Foundation

public class Compiler : NodeVisitor, CustomStringConvertible {
  let classDescription: ClassDescription
  var bytecodes: [Bytecode] = []
  var arguments: [String] = []
  var temporaries: [String] = []
  var selector = ""
  var source = ""

  public var description: String {
    var parts = [classDescription.name]
    parts.append("Method: \(selector)")
    parts.append("Instance variables: \(classDescription.instanceVariables)")
    parts.append("Arguments: \(arguments)")
    parts.append("Temporaries: \(temporaries)")
    parts.append("---------------------------")
    parts.append(source)
    parts.append("===========================")
    parts.append("Bytecodes:")
    for bytecode in bytecodes {
      parts.append("\(bytecode.rawValue) \(bytecode)")
    }
    parts.append("")
    return parts.joined(separator: "\n")
  }

  public init(forClass classDescription: ClassDescription) {
    self.classDescription = classDescription
  }
  public init() {
    self.classDescription = ClassDescription("Test", instanceVariables: [])
  }

  func reset() {
    bytecodes = []
    arguments = []
    temporaries = []
    selector = ""
    source = ""
  }

  public func compileMethod(_ method: String) -> String {
    var output: [String] = []
    source = method.replacingOccurrences(of: "\t", with: "  ")
    let parser = Parser(on: source)
    do {
      let ast = try parser.parseMethod()
      output.append(source)
      output.append("\(ast)")
      reset()
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
    selector = node.selector
    for variableNode in node.arguments {
      arguments.append(variableNode.name)
    }
    if let body = node.body {
      body.accept(self)
    }
  }

  public func visitStatementListNode(_ node: StatementListNode) {
    for variableNode in node.temporaries {
      temporaries.append(variableNode.name)
    }
    // Pragmas
    for statement in node.statements {
      statement.accept(self)
    }
  }

  public func visitReturnNode(_ node: ReturnNode) {
    node.value.accept(self)
    bytecodes.append(.returnTop)
  }

  public func visitMessageNode(_ node: MessageNode) {
    //node.receiver.accept(self)
  }
}
