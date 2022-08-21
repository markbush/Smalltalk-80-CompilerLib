public class CompilerContext : CustomStringConvertible {
  let classDescription: ClassDescription
  var bytecodes: [Bytecode] = []
  var arguments: [String] = []
  var temporaries: [String] = []
  // TODO: replace LiteralValue with Object
  var literals: [LiteralValue] = []
  let specialSelectors = [
    "ifTrue:", "ifFalse:", "ifTrue:ifFalse:", "ifFalse:ifTrue:",
			"and:", "or:", "whileFalse:", "whileTrue:"
  ]
  let specialVars: [String:Bytecode] = [
    "thisContext": .pushContext,
    "self": .pushSelf,
    "super": .pushSelf,
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
    "@": .sendPointAt,
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
                 "Arguments: \(arguments)",
                 "Temporaries: \(temporaries)",
                 "Literals: [\n      \(literalDescriptions.joined(separator: ",\n      "))\n    ]",
                 "===========================",
                 "Bytecodes:"
               ]
    var prev1: Bytecode? = nil
    var prev2: Bytecode? = nil
    for i in 0..<bytecodes.count {
      let bytecode = bytecodes[i]
      let bytecodeInfo = bytecode.describeFor(self, prev1: prev1, prev2: prev2)
      parts.append("\(String(format:"%2d", i)) \(bytecodeInfo)")
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

  public func variableIsKnown(_ variable: String) -> Bool {
    return arguments.contains(variable) || temporaries.contains(variable) || specialVars.keys.contains(variable) || classDescription.instanceVariables.contains(variable)
  }

  func indexForLiteralVariable(_ variable: String) -> Int {
    for index in 0..<literals.count {
      switch literals[index] {
      case .stringVariable(let v, _) where v == variable: return index
      default: break
      }
    }
    let literal = LiteralValue.stringVariable(variable, variable)
    literals.append(literal)
    return literals.count - 1
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
      if index < 64 {
        let extensionCode = index
        guard let bytecode = Bytecode(rawValue: extensionCode) else {
          fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
        }
        push(.pushLong)
        push(bytecode)
        return
      }
      fatalError("Cannot handle more than 64 instance variables for push, needed: \(index)")
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
      if index < 64 {
        let extensionCode = index | 0b01000000
        guard let bytecode = Bytecode(rawValue: extensionCode) else {
          fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
        }
        push(.pushLong)
        push(bytecode)
        return
      }
      fatalError("Cannot handle more than 64 arguments for push, needed: \(index)")
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
      if index < 64 {
        let extensionCode = index | 0b01000000
        guard let bytecode = Bytecode(rawValue: extensionCode) else {
          fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
        }
        push(.pushLong)
        push(bytecode)
        return
      }
      fatalError("Cannot handle more than 64 arguments + temporaries for push, needed: \(index)")
    }
    let variableIndex = indexForLiteralVariable(variable)
    if variableIndex < 32 {
      guard let bytecode = Bytecode(rawValue: Bytecode.pushLiteralVariable0.rawValue + variableIndex) else {
        fatalError("Bytecodes 64-95 (push literal variable) not set up correctly!")
      }
      push(bytecode)
      return
    }
    if variableIndex < 64 {
      let extensionCode = variableIndex | 0b11000000
      guard let bytecode = Bytecode(rawValue: extensionCode) else {
        fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
      }
      push(.pushLong)
      push(bytecode)
      return
    }
    fatalError("Cannot handle more than 64 variables for push, needed: \(variableIndex)")
  }

  public func saveSelectorFor(_ node: MessageNode) {
    let selector = node.selector
    if let _ = specials[selector] , !node.sendsToSuper {
      return
    }
    let literal = LiteralValue.symbolConstant(selector)
    if let _ = literals.firstIndex(of: literal) {
      return
    }
    literals.append(literal)
  }

  public func saveTempVar(_ variable: String) {
    if let _ = arguments.firstIndex(of: variable) {
      return
    }
    if let _ = temporaries.firstIndex(of: variable) {
      return
    }
    temporaries.append(variable)
  }

  func saveLiteralConstant(_ literal: LiteralValue) {
    if literals.firstIndex(of: literal) == nil {
      literals.append(literal)
    }
  }

  func pushLiteralConstant(_ literal: LiteralValue) {
    saveLiteralConstant(literal)
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
    if index < 64 {
      let extensionCode = index | 0b10000000
      guard let bytecode = Bytecode(rawValue: extensionCode) else {
        fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
      }
      push(.pushLong)
      push(bytecode)
      return
    }
    fatalError("Cannot handle more than 64 literals, needed: \(index)")
  }

  public func pushSmallInteger(_ number: Int) {
    let literal = LiteralValue.numConstant(String(number))
    pushLiteralConstant(literal)
  }

  public func saveInteger(_ number: String) {
    let literal = LiteralValue.numConstant(number)
    saveLiteralConstant(literal)
  }

  public func pushInteger(_ number: String) {
    let literal = LiteralValue.numConstant(number)
    pushLiteralConstant(literal)
  }

  public func pushNumber(_ number: String) {
    let literal = LiteralValue.numConstant(number)
    pushLiteralConstant(literal)
  }

  public func saveLiteralString(_ string: String) {
    let literal = LiteralValue.stringConstant(string)
    saveLiteralConstant(literal)
  }

  public func pushLiteralString(_ string: String) {
    let literal = LiteralValue.stringConstant(string)
    pushLiteralConstant(literal)
  }

  public func saveLiteralSymbol(_ string: String) {
    let literal = LiteralValue.symbolConstant(string)
    saveLiteralConstant(literal)
  }

  public func pushLiteralSymbol(_ string: String) {
    let literal = LiteralValue.symbolConstant(string)
    pushLiteralConstant(literal)
  }

  public func saveLiteralCharacter(_ string: String) {
    let literal = LiteralValue.characterConstant(string)
    saveLiteralConstant(literal)
  }

  public func pushLiteralCharacter(_ string: String) {
    let literal = LiteralValue.characterConstant(string)
    pushLiteralConstant(literal)
  }

  public func saveLiteralArray(_ array: String) {
    let literal = LiteralValue.arrayConstant(array)
    saveLiteralConstant(literal)
  }

  public func pushLiteralArray(_ array: String) {
    let literal = LiteralValue.arrayConstant(array)
    pushLiteralConstant(literal)
  }

  func pushSuperSendFor(_ node: MessageNode) {
    let selector = node.selector
    let literal = LiteralValue.symbolConstant(selector)
    if let index = literals.firstIndex(of: literal) {
      if node.numArguments < 8 && index < 0b100000 {
        let arg = (node.numArguments << 5) | index
        guard let bytecode = Bytecode(rawValue: arg) else {
          fatalError("Bytecodes not set up correctly (needed byte \(arg))!")
        }
        push(.sendSuperLong)
        push(bytecode)
        return
      }
      guard let bytecode1 = Bytecode(rawValue: node.numArguments) else {
        fatalError("Bytecodes not set up correctly (needed byte \(node.numArguments))!")
      }
      guard let bytecode2 = Bytecode(rawValue: index) else {
        fatalError("Bytecodes not set up correctly (needed byte \(index))!")
      }
      push(.sendSuperDoubleLong)
      push(bytecode1)
      push(bytecode2)
      return
    }
    fatalError("Cannot find literal for selector \(selector) in literals \(literals)")
  }

  public func pushSelectorFor(_ node: MessageNode) {
    if node.sendsToSuper {
      pushSuperSendFor(node)
      return
    }
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
      if node.numArguments <= 7 && index < 32 {
        let arg = (node.numArguments * 32) + index
        guard let bytecode = Bytecode(rawValue: arg) else {
          fatalError("Bytecodes not set up correctly (needed byte \(arg))!")
        }
        push(.sendLong)
        push(bytecode)
        return
      }
      if node.numArguments < 256 && index < 256 {
        guard let arg1Bytecode = Bytecode(rawValue: node.numArguments) else {
          fatalError("Bytecodes not set up correctly (needed byte \(node.numArguments))!")
        }
        guard let arg2Bytecode = Bytecode(rawValue: index) else {
          fatalError("Bytecodes not set up correctly (needed byte \(index))!")
        }
        push(.sendDoubleLong)
        push(arg1Bytecode)
        push(arg2Bytecode)
        return
      }
      // Should never encounter other sends!
      fatalError("Cannot handle selector: \(selector) at index: \(index)")
    }
    fatalError("Did not find literal for selector: \(selector)")
  }

  public func popVariable(_ variable: String) {
    // Check for args (not supposed to store into args!)
    if let argIndex = arguments.firstIndex(of: variable) {
      let index = argIndex
      if index < 8 {
        guard let bytecode = Bytecode(rawValue: Bytecode.popTemporary0.rawValue + index) else {
          fatalError("Bytecodes 104-111 (pop temporary variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      let extensionCode = index | 0b01000000
      guard let bytecode = Bytecode(rawValue: extensionCode) else {
        fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
      }
      push(.popLong)
      push(bytecode)
      return
    }
    // Check for instance variables
    if let index = classDescription.indexOfInstanceVariable(variable) {
      if index < 8 {
        guard let bytecode = Bytecode(rawValue: Bytecode.popInstanceVar0.rawValue + index) else {
          fatalError("Bytecodes 96-103 (pop instance variable) not set up correctly!")
        }
        push(bytecode)
        return
      }
      guard let bytecode = Bytecode(rawValue: index) else {
        fatalError("Bytecodes not set up correctly (needed byte \(index))!")
      }
      push(.popLong)
      push(bytecode)
      return
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
      let extensionCode = index | 0b01000000
      guard let bytecode = Bytecode(rawValue: extensionCode) else {
        fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
      }
      push(.popLong)
      push(bytecode)
      return
    }
    let index = indexForLiteralVariable(variable)
    if index < 64 {
      let extensionCode = index | 0b11000000
      guard let bytecode = Bytecode(rawValue: extensionCode) else {
        fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
      }
      push(.popLong)
      push(bytecode)
      return
    }
    fatalError("Cannot handle popping to \(variable) with literal index \(index)")
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
      fatalError("Cannot handle more than 64 instance variables yet, need: \(index)")
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
      fatalError("Cannot handle more than 64 temporaries, need: \(index)")
    }
    let index = indexForLiteralVariable(variable)
    if index < 64 {
      let extensionCode = index | 0b11000000
      guard let bytecode = Bytecode(rawValue: extensionCode) else {
        fatalError("Bytecodes not set up correctly (needed byte \(extensionCode))!")
      }
      push(.storeLong)
      push(bytecode)
      return
    }
    fatalError("Cannot handle popping to \(variable) with literal index \(index)")
  }

  public func pushNum(_ num: Int) {
    if num >= -1 && num <= 2 {
      let rawValue = Bytecode.pushZero.rawValue + num
      guard let bytecode = Bytecode(rawValue: rawValue) else {
        fatalError("Bytecodes 116-119 (-1, 0, 1, 2) not set up correctly!")
      }
      push(bytecode)
      return
    }
    pushSmallInteger(num)
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
      let rawValue = Bytecode.jumpOnFalse1.rawValue + numBytes - 1
      guard let bytecode = Bytecode(rawValue: rawValue) else {
        fatalError("Bytecodes 152-159 (jump on false) not set up correctly!")
      }
      push(bytecode)
    }
  }

  public func pushJump(_ numBytes: Int) {
    if numBytes <= 8 {
      let rawValue = Bytecode.jump1.rawValue + numBytes - 1
      guard let bytecode = Bytecode(rawValue: rawValue) else {
        fatalError("Bytecodes 174-181 (jump) not set up correctly!")
      }
      push(bytecode)
      return
    }
    pushLongJump(numBytes)
  }

  public func pushLongJump(_ numBytes: Int) {
    var jump = numBytes / 256
    var offset = numBytes % 256
    while offset < 0 {
      offset += 256
      jump -= 1
    }
    if jump < -4 || jump > 3 {
      fatalError("Jump is too far (\(numBytes))")
    }
    let rawValue = Bytecode.jumpLong4.rawValue + jump
    guard let bytecode = Bytecode(rawValue: rawValue) else {
      fatalError("Bytecodes 174-181 (jump) not set up correctly!")
    }
    push(bytecode)
    guard let offsetBytecode = Bytecode(rawValue: offset) else {
      fatalError("Bytecodes not set up correctly (needed byte \(offset))!")
    }
    push(offsetBytecode)
  }
}
