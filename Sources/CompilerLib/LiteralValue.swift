public enum LiteralValue : Equatable {
case constant(_ value: String)
case variable(_ variable: String, _ value: String)
}
