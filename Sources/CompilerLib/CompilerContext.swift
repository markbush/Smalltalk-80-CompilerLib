public class CompilerContext : CustomStringConvertible {
  let classDescription: ClassDescription
  var bytecodes: [Bytecode] = []
  var arguments: [String] = []
  var temporaries: [String] = []
  // TODO: replace LiteralValue with Object
  var literals: [LiteralValue] = []
  public var selector = ""
  let specialVars: [String:Bytecode] = [
    "self": .pushSelf,
    "true": .pushTrue,
    "false": .pushFalse,
    "nil": .pushNil
  ]
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
      literalDescriptions.append(String(describing: literal))
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
    var prev1: Bytecode? = nil
    var prev2: Bytecode? = nil
    for bytecode in bytecodes {
      let bytecodeInfo = bytecode.describeFor(self, prev1: prev1, prev2: prev2)
      parts.append(bytecodeInfo)
      prev2 = prev1
      prev1 = bytecode
    }
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

  public func returns() -> Bool {
    if bytecodes.count == 0 {
      return false
    }
    switch bytecodes[bytecodes.count-1] {
    case .returnSelf, .returnTrue, .returnFalse, .returnNil, .returnTop: return true
    default: return false
    }
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
    if let bytecode = specialVars[variable] {
      push(bytecode)
      return
    }
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
    let literal = LiteralValue.classVariable(variable, classDescription)
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
    let literal = LiteralValue.symbolConstant(selector)
    if let _ = literals.firstIndex(of: literal) {
      return
    }
    literals.append(literal)
  }

  func pushLiteralConstant(_ literal: LiteralValue) {
    if literals.firstIndex(of: literal) == nil {
      literals.append(literal)
    }
    guard let index = literals.firstIndex(of: literal) else {
      fatalError("Literal wasn't stored!")
    }
    if index < 32 {
      let bytecodeNumber = Bytecode.pushLiteralConstant0.rawValue + index
      guard let bytecode = Bytecode(rawValue: bytecodeNumber) else {
        fatalError("Bytecodes 32-63 (send literal selector) not set up correctly!")
      }
      push(bytecode)
      return
    }
    // TODO: handle more than 32 literals
  }

  public func pushLiteralString(_ string: String) {
    let literal = LiteralValue.stringConstant(string)
    pushLiteralConstant(literal)
  }

  public func pushLiteralCharacter(_ string: String) {
    let literal = LiteralValue.characterConstant(string)
    pushLiteralConstant(literal)
  }

  public func pushSelectorFor(_ node: MessageNode) {
    let selector = node.selector
    if let bytecode = specials[selector] {
      push(bytecode)
      return
    }
    let literal = LiteralValue.symbolConstant(selector)
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

  public func storeVariable(_ variable: String) {
    // Check for instance variables
    if let index = classDescription.indexOfInstanceVariable(variable) {
      if index < 64 {
        guard let bytecode = Bytecode(rawValue: index) else {
          fatalError("Bytecodes not set up correctly (needed byte \(index))!")
        }
        push(.storeLong)
        push(bytecode)
        return
      }
      // TODO: handle more than 64 instance variables
    }
    // Check for temporaries
    if let tempIndex = temporaries.firstIndex(of: variable) {
      // Account for arguments
      let index = tempIndex + arguments.count
      if index < 64 {
        let storeCode = 0b01000000 | index
        guard let bytecode = Bytecode(rawValue: storeCode) else {
          fatalError("Bytecodes not set up correctly (needed byte \(index))!")
        }
        push(.storeLong)
        push(bytecode)
        return
      }
      // TODO: handle more than 64 arguments+temporaries
    }
  }

  public func pushConditionalJumpOn(_ isTrueCondition: Bool, numBytes: Int) {
    if numBytes > 8 || isTrueCondition {
      let jumpBlock = numBytes / 256
      let offset = numBytes % 256
      if jumpBlock > 3 {
        fatalError("Block too long!")
      }
      let baseValue = isTrueCondition ? Bytecode.jumpLongOnTrue0 : Bytecode.jumpLongOnFalse0
      let rawValue = baseValue.rawValue + jumpBlock
      guard let bytecode = Bytecode(rawValue: rawValue) else {
        fatalError("Bytecodes 168-175 (jump long on false) not set up correctly!")
      }
      push(bytecode)
      guard let offsetBytecode = Bytecode(rawValue: offset) else {
        fatalError("Bytecodes not set up correctly (needed byte \(offset))!")
      }
      push(offsetBytecode)
    } else {
      let rawValue = Bytecode.jumpOnFalse1.rawValue + numBytes - 2
      guard let bytecode = Bytecode(rawValue: rawValue) else {
        fatalError("Bytecodes 152-159 (jump on false) not set up correctly!")
      }
      push(bytecode)
    }
  }
}
