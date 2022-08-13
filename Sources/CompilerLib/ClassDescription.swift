public class ClassDescription : Equatable {
  let name: String
  let instanceVariables: [String]

  public static func == (lhs: ClassDescription, rhs: ClassDescription) -> Bool {
    return lhs.name == rhs.name
  }

  public init(_ name: String, instanceVariables instVars: [String]) {
    self.name = name
    self.instanceVariables = instVars
  }

  public func indexOfInstanceVariable(_ variable: String) -> Int? {
    return instanceVariables.firstIndex(of: variable)
  }
}
