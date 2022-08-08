public class ClassDescription {
  let name: String
  let instanceVariables: [String]

  public init(_ name: String, instanceVariables instVars: [String]) {
    self.name = name
    self.instanceVariables = instVars
  }
}
