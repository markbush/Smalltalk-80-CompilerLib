public class CompilerContext : CustomStringConvertible {
  let classDescription: ClassDescription
  var bytecodes: [Bytecode] = []
  var arguments: [String] = []
  var temporaries: [String] = []
  var literals: [LiteralValue] = []
  public var selector = ""
  let specials: [String:Bytecode] = [
    "+": .sendPlus,
    "-": .sendMinus,
    "<": .sendLessThan,
    ">": .sendGreaterThan,
    "<=": .sendLessOrEqual,
    ">=": .sendGreaterOrEqual,
    "=": .sendEquals,
    "~=": .sendNotEqual,
    "*": .sendMultiply,
    "/": .sendDivide,
    "\\\\": .sendMod,
    "@": .sendAt,
    "bitShift:": .sendBitShift,
    "//": .sendDiv,
    "bitAnd:": .sendBitAnd,
    "bitOr:": .sendBitOr,
    "at:": .sendAt,
    "at:put:": .sendAtPut,
    "size": .sendSize,
    "next": .sendNext,
    "nextPut:": .sendNextPut,
    "atEnd": .sendAtEnd,
    "==": .sendIdenticalTo,
    "class": .sendClass,
    "blockCopy:": .sendBlockCopy,
    "value": .sendValue,
    "value:": .sendValueWithArg,
    "do:": .sendDo,
    "new": .sendNew,
    "new:": .sendNewWithArg,
    "x": .sendX,
    "y": .sendY
  ]

  public var description: String {
    var literalDescriptions: [String] = []
    for literal in literals {
      switch literal {
      case .constant(let value): literalDescriptions.append(value)
      case .variable(let variable, let value): literalDescriptions.append("\(variable) -> \(value)")
      }
    }
    var parts = [classDescription.name,
                 "Instance variables: \(classDescription.instanceVariables)",
                 "Method: \(selector)",
                 "Arguments: \(arguments)",
                 "Temporaries: \(temporaries)",
                 "Literals: \(literalDescriptions)",
                 "===========================",
                 "Bytecodes:"
               ]
    for bytecode in bytecodes {
      parts.append("\(bytecode.rawValue) \(bytecode)")
    }
    parts.append("")
    return parts.joined(separator: "\n")
  }

  public init(forClass classDescription: ClassDescription) {
    self.classDescription = classDescription
  }

  public func reset() {
    bytecodes = []
    arguments = []
    temporaries = []
    literals = []
    selector = ""
  }

  public func addArg(_ argument: String) {
    arguments.append(argument)
  }

  public func addTemp(_ temporary: String) {
    temporaries.append(temporary)
  }

  public func push(_ bytecode: Bytecode) {
    bytecodes.append(bytecode)
  }

  public func pushVariable(_ variable: String) {
    // Check for instance variables
    if let index = classDescription.indexOfInstanceVariable(variable) {
      if index < 16 {
        guard let bytecode = Bytecode(rawValue: Bytecode.pushInstVar0.rawValue + index) else {
          fatalError("Bytecodes 0-15 (push instance variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      // TODO: handle more than 16 instance variables
    }
    // Check method arguments
    if let index = arguments.firstIndex(of: variable) {
      if index < 16 {
        guard let bytecode = Bytecode(rawValue: Bytecode.pushTemporary0.rawValue + index) else {
          fatalError("Bytecodes 16-31 (push temporary variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      // TODO: handle more than 16 arguments
    }
    // Check temporaries
    if let tempIndex = temporaries.firstIndex(of: variable) {
      // Account for arguments
      let index = tempIndex + arguments.count
      if index < 16 {
        guard let bytecode = Bytecode(rawValue: Bytecode.pushTemporary0.rawValue + index) else {
          fatalError("Bytecodes 16-31 (push temporary variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      // TODO: handle more than 16 arguments+temporaries
    }
    // Check for literal variables
    var literalIndex: Int? = nil
    // TODO: lookup global
    let literal = LiteralValue.variable(variable, variable)
    if let index = literals.firstIndex(of: literal) {
      literalIndex = index
    } else {
      literals.append(literal)
      literalIndex = literals.count - 1
    }
    guard let variableIndex = literalIndex else {
      fatalError("Literal index must have been set!")
    }
    if variableIndex < 32 {
      guard let bytecode = Bytecode(rawValue: Bytecode.pushLiteralVariable0.rawValue + variableIndex) else {
        fatalError("Bytecodes 64-95 (push literal variable) not set up correctly!")
      }
      push(bytecode)
      return
    }
    // TODO: handle more than 32 literals
    // TODO: handle all globals
  }

  public func saveSelectorFor(_ node: MessageNode) {
    let selector = node.selector
    if let _ = specials[selector] {
      return
    }
    let literal = LiteralValue.constant(selector)
    if let _ = literals.firstIndex(of: literal) {
      return
    }
    literals.append(literal)
  }

  public func pushSelectorFor(_ node: MessageNode) {
    let selector = node.selector
    if let bytecode = specials[selector] {
      push(bytecode)
      return
    }
    let literal = LiteralValue.constant(selector)
    if let index = literals.firstIndex(of: literal) {
      if node.numArguments <= 2 && index < 16 {
        let bytecodeNumber = Bytecode.sendNoArgLiteral0.rawValue + (node.numArguments * 16) + index
        guard let bytecode = Bytecode(rawValue: bytecodeNumber) else {
          fatalError("Bytecodes 208-255 (send literal selector) not set up correctly!")
        }
        push(bytecode)
        return
      }
      // TODO: handle other sends
    }
  }

  public func popVariable(_ variable: String) {
    // Check for instance variables
    if let index = classDescription.indexOfInstanceVariable(variable) {
      if index < 8 {
        guard let bytecode = Bytecode(rawValue: Bytecode.popInstanceVar0.rawValue + index) else {
          fatalError("Bytecodes 96-103 (pop instance variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      // TODO: handle more than 8 instance variables
    }
    // Check for temporaries
    if let tempIndex = temporaries.firstIndex(of: variable) {
      // Account for arguments
      let index = tempIndex + arguments.count
      if index < 8 {
        guard let bytecode = Bytecode(rawValue: Bytecode.popTemporary0.rawValue + index) else {
          fatalError("Bytecodes 104-111 (pop temporary variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      // TODO: handle more than 8 arguments+temporaries
    }
  }
}
