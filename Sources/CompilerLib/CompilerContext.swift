public class CompilerContext : CustomStringConvertible {
  let classDescription: ClassDescription
  var bytecodes: [Bytecode] = []
  var arguments: [String] = []
  var temporaries: [String] = []
  var literals: [String] = []
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
    var parts = [classDescription.name,
                 "Instance variables: \(classDescription.instanceVariables)",
                 "Method: \(selector)",
                 "Arguments: \(arguments)",
                 "Temporaries: \(temporaries)",
                 "Literals: \(literals)",
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

  func pushLiteral(_ index: Int) {
    if index < 32 {
      guard let bytecode = Bytecode(rawValue: Bytecode.pushLiteralConstant0.rawValue + index) else {
        fatalError("Bytecodes 32-63 (push literal constant) not set up correctly!")
      }
      push(bytecode)
    }
    // TODO: extended literal push
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
    // TODO: arguments
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
    // TODO: temporaries
  }

  public func pushSelector(_ selector: String) {
    if let bytecode = specials[selector] {
      push(bytecode)
      return
    }
    if let index = literals.firstIndex(of: selector) {
      pushLiteral(index)
      return
    }
    literals.append(selector)
    pushLiteral(literals.count - 1)
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
    // TODO: temporaries
  }
}
