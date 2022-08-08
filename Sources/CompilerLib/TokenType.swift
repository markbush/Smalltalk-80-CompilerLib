public enum TokenType : Equatable {
case eof
case identifier
case keyword
case blockArg
case binary
case string
case symbol
case comment
case number
case character
case openArray
case openByteArray
case openParen
case closeParen
case openSquareParen
case closeSquareParen
case openCurlyParen
case closeCurlyParen
case assign
case period
case caret
case semiColon
case comma
case unknown
case error
}
