public enum LiteralValue : Equatable, CustomStringConvertible {
case stringConstant(_ value: String)
case symbolConstant(_ value: String)
case characterConstant(_ value: String)
case intConstant(_ value: String)
case stringVariable(_ variable: String, _ value: String)
case classVariable(_ variable: String, _ value: ClassDescription)
case intVariable(_ variable: String, _ value: Int)
  public var description: String {
    switch self {
    case .stringConstant(let value): return value
    case .symbolConstant(let value): return value
    case .characterConstant(let value): return value
    case .intConstant(let value): return String(value)
    case .stringVariable(let variable, let value): return "\(variable) -> \(value)"
    case .classVariable(let variable, let value): return "\(variable) -> \(value.name)"
    case .intVariable(let variable, let value): return "\(variable) -> \(value)"
    }
  }
}
