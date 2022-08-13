public enum Bytecode : Int {
case pushInstVar0 = 0x00
case pushInstVar1 = 0x01
case pushInstVar2 = 0x02
case pushInstVar3 = 0x03
case pushInstVar4 = 0x04
case pushInstVar5 = 0x05
case pushInstVar6 = 0x06
case pushInstVar7 = 0x07
case pushInstVar8 = 0x08
case pushInstVar9 = 0x09
case pushInstVarA = 0x0A
case pushInstVarB = 0x0B
case pushInstVarC = 0x0C
case pushInstVarD = 0x0D
case pushInstVarE = 0x0E
case pushInstVarF = 0x0F
case pushTemporary0 = 0x10
case pushTemporary1 = 0x11
case pushTemporary2 = 0x12
case pushTemporary3 = 0x13
case pushTemporary4 = 0x14
case pushTemporary5 = 0x15
case pushTemporary6 = 0x16
case pushTemporary7 = 0x17
case pushTemporary8 = 0x18
case pushTemporary9 = 0x19
case pushTemporaryA = 0x1a
case pushTemporaryB = 0x1b
case pushTemporaryC = 0x1c
case pushTemporaryD = 0x1d
case pushTemporaryE = 0x1e
case pushTemporaryF = 0x1f
case pushLiteralConstant0 = 0x20
case pushLiteralConstant1 = 0x21
case pushLiteralConstant2 = 0x22
case pushLiteralConstant3 = 0x23
case pushLiteralConstant4 = 0x24
case pushLiteralConstant5 = 0x25
case pushLiteralConstant6 = 0x26
case pushLiteralConstant7 = 0x27
case pushLiteralConstant8 = 0x28
case pushLiteralConstant9 = 0x29
case pushLiteralConstantA = 0x2A
case pushLiteralConstantB = 0x2B
case pushLiteralConstantC = 0x2C
case pushLiteralConstantD = 0x2D
case pushLiteralConstantE = 0x2E
case pushLiteralConstantF = 0x2F
case pushLiteralConstant10 = 0x30
case pushLiteralConstant11 = 0x31
case pushLiteralConstant12 = 0x32
case pushLiteralConstant13 = 0x33
case pushLiteralConstant14 = 0x34
case pushLiteralConstant15 = 0x35
case pushLiteralConstant16 = 0x36
case pushLiteralConstant17 = 0x37
case pushLiteralConstant18 = 0x38
case pushLiteralConstant19 = 0x39
case pushLiteralConstant1A = 0x3A
case pushLiteralConstant1B = 0x3B
case pushLiteralConstant1C = 0x3C
case pushLiteralConstant1D = 0x3D
case pushLiteralConstant1E = 0x3E
case pushLiteralConstant1F = 0x3F
case pushLiteralVariable0 = 0x40
case pushLiteralVariable1 = 0x41
case pushLiteralVariable2 = 0x42
case pushLiteralVariable3 = 0x43
case pushLiteralVariable4 = 0x44
case pushLiteralVariable5 = 0x45
case pushLiteralVariable6 = 0x46
case pushLiteralVariable7 = 0x47
case pushLiteralVariable8 = 0x48
case pushLiteralVariable9 = 0x49
case pushLiteralVariableA = 0x4A
case pushLiteralVariableB = 0x4B
case pushLiteralVariableC = 0x4C
case pushLiteralVariableD = 0x4D
case pushLiteralVariableE = 0x4E
case pushLiteralVariableF = 0x4F
case pushLiteralVariable10 = 0x50
case pushLiteralVariable11 = 0x51
case pushLiteralVariable12 = 0x52
case pushLiteralVariable13 = 0x53
case pushLiteralVariable14 = 0x54
case pushLiteralVariable15 = 0x55
case pushLiteralVariable16 = 0x56
case pushLiteralVariable17 = 0x57
case pushLiteralVariable18 = 0x58
case pushLiteralVariable19 = 0x59
case pushLiteralVariable1A = 0x5A
case pushLiteralVariable1B = 0x5B
case pushLiteralVariable1C = 0x5C
case pushLiteralVariable1D = 0x5D
case pushLiteralVariable1E = 0x5E
case pushLiteralVariable1F = 0x5F
case popInstanceVar0 = 0x60
case popInstanceVar1 = 0x61
case popInstanceVar2 = 0x62
case popInstanceVar3 = 0x63
case popInstanceVar4 = 0x64
case popInstanceVar5 = 0x65
case popInstanceVar6 = 0x66
case popInstanceVar7 = 0x67
case popTemporary0 = 0x68
case popTemporary1 = 0x69
case popTemporary2 = 0x6A
case popTemporary3 = 0x6B
case popTemporary4 = 0x6C
case popTemporary5 = 0x6D
case popTemporary6 = 0x6E
case popTemporary7 = 0x6F
case pushSelf = 0x70
case pushTrue = 0x71
case pushFalse = 0x72
case pushNil = 0x73
case pushMinusOne = 0x74
case pushZero = 0x75
case pushOne = 0x76
case pushTwo = 0x77
case returnSelf = 0x78
case returnTrue = 0x79
case returnFalse = 0x7A
case returnNil = 0x7B
case returnTop = 0x7C
case pushLong = 0x80
case storeLong = 0x81
case popLong = 0x82
case sendLong = 0x83
case sendDoubleLong = 0x84
case sendSuperLong = 0x85
case sendSuperDoubleLong = 0x86
case popStack = 0x87
case dupTop = 0x88
case pushContext = 0x89
case jump1 = 0x90
case jump2 = 0x91
case jump3 = 0x92
case jump4 = 0x93
case jump5 = 0x94
case jump6 = 0x95
case jump7 = 0x96
case jump8 = 0x97
case jumpOnFalse1 = 0x98
case jumpOnFalse2 = 0x99
case jumpOnFalse3 = 0x9A
case jumpOnFalse4 = 0x9B
case jumpOnFalse5 = 0x9C
case jumpOnFalse6 = 0x9D
case jumpOnFalse7 = 0x9E
case jumpOnFalse8 = 0x9F
case jumpLong0 = 0xA0 // 1 extension (0-4)*256 + next
case jumpLong1 = 0xA1 // 1 extension (1-4)*256 + next
case jumpLong2 = 0xA2 // 1 extension (2-4)*256 + next
case jumpLong3 = 0xA3 // 1 extension (3-4)*256 + next
case jumpLong4 = 0xA4 // 1 extension (4-4)*256 + next
case jumpLong5 = 0xA5 // 1 extension (5-4)*256 + next
case jumpLong6 = 0xA6 // 1 extension (6-4)*256 + next
case jumpLong7 = 0xA7 // 1 extension (7-4)*256 + next
case jumpLongOnTrue0 = 0xA8 // 1 extension 0*256 + next
case jumpLongOnTrue1 = 0xA9 // 1 extension 1*256 + next
case jumpLongOnTrue2 = 0xAA // 1 extension 2*256 + next
case jumpLongOnTrue3 = 0xAB // 1 extension 3*256 + next
case jumpLongOnFalse0 = 0xAC // 1 extension 0*256 + next
case jumpLongOnFalse1 = 0xAD // 1 extension 1*256 + next
case jumpLongOnFalse2 = 0xAE // 1 extension 2*256 + next
case jumpLongOnFalse3 = 0xAF // 1 extension 3*256 + next
case sendPlus = 0xB0
case sendMinus = 0xB1
case sendLessThan = 0xB2
case sendGreaterThan = 0xB3
case sendLessOrEqual = 0xB4
case sendGreaterOrEqual = 0xB5
case sendEquals = 0xB6
case sendNotEqual = 0xB7
case sendMultiply = 0xB8
case sendDivide = 0xB9
case sendMod = 0xBA
case sendPointAt = 0xBB
case sendBitShift = 0xBC
case sendDiv = 0xBD
case sendBitAnd = 0xBE
case sendBitOr = 0xBF
case sendAt = 0xC0
case sendAtPut = 0xC1
case sendSize = 0xC2
case sendNext = 0xC3
case sendNextPut = 0xC4
case sendAtEnd = 0xC5
case sendIdenticalTo = 0xC6
case sendClass = 0xC7
case sendBlockCopy = 0xC8
case sendValue = 0xC9
case sendValueWithArg = 0xCA
case sendDo = 0xCB
case sendNew = 0xCC
case sendNewWithArg = 0xCD
case sendX = 0xCE
case sendY = 0xCF
case sendNoArgLiteral0 = 0xD0
case sendNoArgLiteral1 = 0xD1
case sendNoArgLiteral2 = 0xD2
case sendNoArgLiteral3 = 0xD3
case sendNoArgLiteral4 = 0xD4
case sendNoArgLiteral5 = 0xD5
case sendNoArgLiteral6 = 0xD6
case sendNoArgLiteral7 = 0xD7
case sendNoArgLiteral8 = 0xD8
case sendNoArgLiteral9 = 0xD9
case sendNoArgLiteralA = 0xDA
case sendNoArgLiteralB = 0xDB
case sendNoArgLiteralC = 0xDC
case sendNoArgLiteralD = 0xDD
case sendNoArgLiteralE = 0xDE
case sendNoArgLiteralF = 0xDF
case sendOneArgLiteral0 = 0xE0
case sendOneArgLiteral1 = 0xE1
case sendOneArgLiteral2 = 0xE2
case sendOneArgLiteral3 = 0xE3
case sendOneArgLiteral4 = 0xE4
case sendOneArgLiteral5 = 0xE5
case sendOneArgLiteral6 = 0xE6
case sendOneArgLiteral7 = 0xE7
case sendOneArgLiteral8 = 0xE8
case sendOneArgLiteral9 = 0xE9
case sendOneArgLiteralA = 0xEA
case sendOneArgLiteralB = 0xEB
case sendOneArgLiteralC = 0xEC
case sendOneArgLiteralD = 0xED
case sendOneArgLiteralE = 0xEE
case sendOneArgLiteralF = 0xEF
case sendTwoArgLiteral0 = 0xF0
case sendTwoArgLiteral1 = 0xF1
case sendTwoArgLiteral2 = 0xF2
case sendTwoArgLiteral3 = 0xF3
case sendTwoArgLiteral4 = 0xF4
case sendTwoArgLiteral5 = 0xF5
case sendTwoArgLiteral6 = 0xF6
case sendTwoArgLiteral7 = 0xF7
case sendTwoArgLiteral8 = 0xF8
case sendTwoArgLiteral9 = 0xF9
case sendTwoArgLiteralA = 0xFA
case sendTwoArgLiteralB = 0xFB
case sendTwoArgLiteralC = 0xFC
case sendTwoArgLiteralD = 0xFD
case sendTwoArgLiteralE = 0xFE
case sendTwoArgLiteralF = 0xFF
  public func describeFor(_ context: CompilerContext, prev1: Bytecode? = nil, prev2: Bytecode? = nil) -> String {
    var result = "\(String(format:"%02X", rawValue))  \(String(format:"%3d", rawValue))"
    if prev2 == .sendDoubleLong || prev2 == .sendSuperDoubleLong {
      return result
    }
    switch prev1 {
    case .pushLong, .storeLong, .popLong, .sendLong, .sendSuperLong: return result
    default: break
    }
    if prev1 != nil && prev1!.rawValue >= Bytecode.jumpLong0.rawValue && prev1!.rawValue <= Bytecode.jumpLongOnFalse3.rawValue {
      return result
    }
    result.append(" \(self)")
    switch rawValue {
    case Bytecode.pushInstVar0.rawValue ... Bytecode.pushInstVarF.rawValue:
      let index = rawValue
      result.append(instVarIn(context, at: index))
    case Bytecode.popInstanceVar0.rawValue ... Bytecode.popInstanceVar7.rawValue:
      let index = rawValue - Bytecode.popInstanceVar0.rawValue
      result.append(instVarIn(context, at: index))
    case Bytecode.pushTemporary0.rawValue ... Bytecode.pushTemporaryF.rawValue:
      let index = rawValue - Bytecode.pushTemporary0.rawValue
      result.append(tempIn(context, at: index))
    case Bytecode.popTemporary0.rawValue ... Bytecode.popTemporary7.rawValue:
      let index = rawValue - Bytecode.popTemporary0.rawValue
      result.append(tempIn(context, at: index))
    case Bytecode.pushLiteralConstant0.rawValue ... Bytecode.pushLiteralConstant1F.rawValue:
      let index = rawValue - Bytecode.pushLiteralConstant0.rawValue
      result.append(literalStringIn(context, at: index))
    case Bytecode.pushLiteralVariable0.rawValue ... Bytecode.pushLiteralVariable1F.rawValue:
      let index = rawValue - Bytecode.pushLiteralVariable0.rawValue
      result.append(literalStringIn(context, at: index))
    case Bytecode.sendNoArgLiteral0.rawValue ... Bytecode.sendTwoArgLiteralF.rawValue:
      let index = (rawValue - Bytecode.sendNoArgLiteral0.rawValue) % 16
      result.append(literalStringIn(context, at: index))
    default: break
    }
    return result
  }
  func literalStringIn(_ context: CompilerContext, at index: Int) -> String {
    if index >= context.literals.count {
      print("Literal index \(index) out of range for literals \(context.literals) in \(self)")
    } else {
      if case let .stringConstant(constant) = context.literals[index] {
        return " ('\(constant)')"
      }
      if case let .symbolConstant(constant) = context.literals[index] {
        return " (#\(constant))"
      }
      if case let .characterConstant(constant) = context.literals[index] {
        return " ($\(constant))"
      }
      if case let .classVariable(variable, _) = context.literals[index] {
        return " (\(variable))"
      }
    }
    return ""
  }
  func instVarIn(_ context: CompilerContext, at index: Int) -> String {
    if index >= context.classDescription.instanceVariables.count {
      print("Inst var index \(index) out of range for inst vars \(context.classDescription.instanceVariables) in \(self)")
    } else {
      let constant = context.classDescription.instanceVariables[index]
      return " (\(constant))"
    }
    return ""
  }
  func tempIn(_ context: CompilerContext, at index: Int) -> String {
    let allTemps = context.arguments + context.temporaries
    if index >= allTemps.count {
      print("Temps index \(index) out of range for temps \(allTemps) in \(self)")
    } else {
      let constant = allTemps[index]
      return " (\(constant))"
    }
    return ""
  }
}
