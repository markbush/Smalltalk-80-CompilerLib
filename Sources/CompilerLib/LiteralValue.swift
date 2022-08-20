public enum LiteralValue : Equatable, CustomStringConvertible {
case stringConstant(_ value: String)
case symbolConstant(_ value: String)
case characterConstant(_ value: String)
case numConstant(_ value: String)
case stringVariable(_ variable: String, _ value: String)
case classVariable(_ variable: String, _ value: ClassDescription)
case intVariable(_ variable: String, _ value: Int)
case arrayConstant(_ value: String)
  public var description: String {
    switch self {
    case .stringConstant(let value): return ".stringConstant(\"\(value)\")"
    case .symbolConstant(let value): return ".symbolConstant(\"\(value)\")"
    case .characterConstant(let value): return ".characterConstant(\"\(value)\")"
    case .numConstant(let value): return ".numConstant(\"\(value)\")"
    case .stringVariable(let variable, let value): return ".stringVariable(\"\(variable)\", \"\(value)\")"
    case .classVariable(let variable, let value): return ".classVariable(\"\(variable)\", \"\(value.name)\")"
    case .intVariable(let variable, let value): return ".intVariable(\"\(variable)\", \"\(value)\")"
    case .arrayConstant(let value): return ".arrayConstant(\"\(value)\")"
    }
  }
}
