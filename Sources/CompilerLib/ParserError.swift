public enum ParserError: Error {
case syntaxError(reason: String, position: Int?)
}
