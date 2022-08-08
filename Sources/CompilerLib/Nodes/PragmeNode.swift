public class PragmaNode : Node {
  let selector: String
  var arguments: [String] = []

  public override var description: String {
    var parts = ["PragmaNode:",
    "      Selector: #\(selector)",
    "      Arguments: \(arguments)"]
    if comments.count > 0 {
      parts.append("Comments: \"\(comments.joined(separator: "\n"))\"\n")
    }
    return "("+String(parts.joined(separator: "\n"))+")"
  }

  public convenience init(_ selector: String) {
    self.init(selector, withArgs: [])
  }

  public init(_ selector: String, withArgs arguments: [String]) {
    self.selector = selector
    self.arguments = arguments
  }
}
