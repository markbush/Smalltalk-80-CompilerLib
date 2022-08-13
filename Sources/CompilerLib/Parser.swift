public class Parser {
  let source: String
  public private(set) var tokens: [Token] = []
  public private(set) var currentToken: Token? = nil
  var comments: [String] = []
  public private(set) var position = -1

  public init(on source: String) {
    self.source = source
    initialise()
    step()
  }

  func initialise() {
    let scanner = Scanner(on: source)
    while true {
      let token = scanner.nextToken()
      if token.type == .unknown || token.type == .eof {
        break
      }
      tokens.append(token)
    }
    comments = []
    position = -1
  }

  func step() {
    position += 1
    if position >= tokens.count {
      currentToken = nil
    } else {
      currentToken = tokens[position]
      comments.append(contentsOf: currentToken!.comments)
    }
  }

  func resetTo(_ newPosition: Int) {
    position = newPosition
    currentToken = (position < tokens.count) ? tokens[position] : nil
  }

  func atEndOfExpression() -> Bool {
    if let token = currentToken, token.type == .period || token.type == .closeSquareParen || token.type == .closeParen || token.type == .semiColon || token.type == .closeCurlyParen {
      return true
    }
    return false
  }

  func storeCommentsTo(_ node: Node) {
    node.comments.append(contentsOf: comments)
    comments = []
  }

  public func parseMethod() throws -> MethodNode {
    let methodNode = try parseMethodSignature()
    storeCommentsTo(methodNode)
    let statementListNode = try parseBody()
    statementListNode.parent = methodNode
    methodNode.body = statementListNode
    return methodNode
  }

  func parseMethodSignature() throws -> MethodNode {
    guard let token = currentToken else {
      throw parserError(expecting: "method signature")
    }
    switch token.type {
    case .identifier: return try parseUnaryMethodSignature()
    case .binary: return try parseBinaryMethodSignature()
    case .keyword: return try parseKeywordMethodSignature()
    default: throw parserError(expecting: "method signature")
    }
  }

  func parseUnaryMethodSignature() throws -> MethodNode {
    guard let identifierToken = currentToken as? ValueToken, identifierToken.type == .identifier else {
      throw parserError(expecting: "identifier")
    }
    let selector = identifierToken.value
    step()
    return MethodNode(selector)
  }

  func parseBinaryMethodSignature() throws -> MethodNode {
    guard let binaryToken = currentToken as? ValueToken, binaryToken.type == .binary else {
      throw parserError(expecting: "binary selector")
    }
    let selector = binaryToken.value
    let node = MethodNode(selector)
    step()
    guard position < tokens.count else {
      throw parserError(expecting: "identifier")
    }
    if let argument = parseIdentifier() {
      node.arguments.append(argument)
      return node
    }
    throw parserError(expecting: "identifier")
  }

  func parseKeywordMethodSignature() throws -> MethodNode {
    var keywords: [String] = []
    var arguments: [VariableNode] = []
    loop: while position < tokens.count {
      if let keywordToken = currentToken as? ValueToken, keywordToken.type == .keyword {
        keywords.append(keywordToken.value)
        step()
        guard position < tokens.count else {
          throw parserError(expecting: "identifier")
        }
        if let identifierNode = parseIdentifier() {
          arguments.append(identifierNode)
        } else {
          throw parserError(expecting: "identifier")
        }
      } else {
        break loop
      }
    }
    let node = MethodNode(keywords.joined())
    node.arguments = arguments
    return node
  }

  func parseBody() throws -> StatementListNode {
    let statementListNode = StatementListNode()
    try parseTemporariesAndPragmasInto(statementListNode)
    while position < tokens.count {
      if let endBlock = currentToken, endBlock.type == .closeSquareParen {
        // close of block
        step()
        break
      }
      if let endToken = currentToken, endToken.type == .period {
        // Empty statement - ignore
        step()
        continue
      }
      let savedPosition = position
      guard let statementNode = try parseStatement() else {
        resetTo(savedPosition)
        throw parserError(expecting: "statement")
      }
      statementListNode.statements.append(statementNode)
      statementNode.parent = statementListNode
      if position >= tokens.count {
        break
      }
      if let endToken = currentToken, endToken.type == .period {
        step()
        continue
      }
      guard let endBlockToken = currentToken, endBlockToken.type == .closeSquareParen else {
        throw parserError(expecting: "end of statement")
      }
    }
    return statementListNode
  }

  func parseTemporariesAndPragmasInto(_ node: StatementListNode) throws {
    guard let token = currentToken as? ValueToken else {
      return
    }

    switch token.type {
    case .binary where token.value == "|":
      step()
      try parseTemporariesInto(node)
      if let pragmaStartToken = currentToken as? ValueToken, pragmaStartToken.value == "<" {
        step()
        try parsePragmasInto(node)
      }
    case .binary where token.value == "<":
      step()
      try parsePragmasInto(node)
      if let temporariesStartToken = currentToken as? ValueToken, temporariesStartToken.value == "|" {
        step()
        try parseTemporariesInto(node)
      }
    default: break
    }
  }

  func parseTemporariesInto(_ node: StatementListNode) throws {
    while position < tokens.count {
      if let endToken = currentToken as? ValueToken, endToken.type == .binary && endToken.value == "|" {
        step()
        break
      }
      if let variableNode = parseIdentifier() {
        node.temporaries.append(variableNode)
        variableNode.parent = node
      } else {
        throw parserError(expecting: "identifier")
      }
    }
  }

  func parsePragmasInto(_ node: StatementListNode) throws {
    var keywords: [String] = []
    var arguments: [String] = []
    while position < tokens.count {
      guard let token = currentToken as? ValueToken else {
        throw parserError(expecting: "pragma")
      }
      switch token.type {
      case .keyword:
        keywords.append(token.value)
      case .binary where token.value == ">":
        step()
        let selector = String(keywords.joined())
        let pragma = PragmaNode(selector, withArgs: arguments)
        node.pragmas.append(pragma)
        pragma.parent = node
        return
      default:
        arguments.append(token.value) // FIXME - should scan for expression
      }
      step()
    }
  }

  func parseIdentifier() -> VariableNode? {
    guard let identifierToken = currentToken as? ValueToken, identifierToken.type == .identifier else {
      return nil
    }
    step()
    return VariableNode(identifierToken.value)
  }

  func parseStatement() throws -> StatementNode? {
    guard let token = currentToken else {
      return nil
    }

    if token.type == .caret {
      step()
      return try parseReturn()
    } else {
      return try parseExpression()
    }
  }

  func parseExpression() throws -> ExpressionNode {
    guard let token = currentToken else {
      throw parserError(expecting: "expression")
    }

    if token.type == .identifier && position+1 < tokens.count && tokens[position+1].type == .assign {
      return try parseAssign()
    }
    // Expression must start with a primary
    let primary = try parsePrimary()
    guard let messageExpression = try parseMessageExpression(receiver: primary) else {
      throw parserError(expecting: "expression")
    }
    guard let cascadeToken = currentToken, cascadeToken.type == .semiColon else {
      return messageExpression
    }
    step()
    let cascadeMessage = CascadeMessageNode()
    cascadeMessage.messages.append(messageExpression)
    try parseCascadeMessagesInto(cascadeMessage, receiver: primary)
    return cascadeMessage
  }

  func parseCascadeMessagesInto(_ cascadeMessage: CascadeMessageNode, receiver: ExpressionNode) throws {
    while position < tokens.count {
      if let endToken = currentToken, endToken.type == .period || endToken.type == .closeSquareParen || endToken.type == .closeParen {
        return
      }
      let savedPosition = position
      guard let messageExpression = try parseMessageExpression(receiver: receiver) else {
        resetTo(savedPosition)
        throw parserError(expecting: "expression")
      }
      cascadeMessage.messages.append(messageExpression)
      messageExpression.parent = cascadeMessage
      guard let cascadeToken = currentToken, cascadeToken.type == .semiColon else {
        return
      }
      step()
    }
  }

  func parseReturn() throws -> ReturnNode {
    let expressionNode = try parseExpression()
    let returnNode = ReturnNode(expressionNode)
    expressionNode.parent = returnNode
    return returnNode
  }

  func parseAssign() throws -> AssignNode {
    guard let variableNode = parseIdentifier() else {
      throw parserError(expecting: "identifier")
    }
    let node = AssignNode()
    node.variable = variableNode
    variableNode.parent = node
    guard let assignToken = currentToken, assignToken.type == .assign else {
      throw parserError(expecting: "':='")
    }
    step()
    let statementNode = try parseExpression()
    node.value = statementNode
    statementNode.parent = node
    return node
  }

  func parsePrimary() throws -> ExpressionNode {
    if let variableNode = parseIdentifier() {
      return variableNode
    }
    let savedPosition = position
    if let blockNode = try parseBlock() {
      return blockNode
    }
    resetTo(savedPosition)
    if let literalNode = try parseLiteral() {
      return literalNode
    }
    resetTo(savedPosition)
    if let parenExpression = try parseParenExpression() {
      return parenExpression
    }
    resetTo(savedPosition)
    if let dynamicArrayExpression = try parseDynamicArray() {
      return dynamicArrayExpression
    }
    resetTo(savedPosition)
    throw parserError(expecting: "primary")
  }

  func parseBlock() throws -> ExpressionNode? {
    guard let token = currentToken, token.type == .openSquareParen else {
      return nil
    }
    step()
    let blockNode = BlockNode()
    try parseBlockArgsInto(blockNode)
    let statements = try parseBody()
    blockNode.body = statements
    statements.parent = blockNode
    return blockNode
  }

  func parseBlockArgsInto(_ node: BlockNode) throws {
    guard let blockArg = currentToken, blockArg.type == .blockArg else {
      // No block args
      return
    }
    while position < tokens.count {
      if let endOfArgs = currentToken as? ValueToken, endOfArgs.type == .binary && endOfArgs.value == "|" {
        // Is it the end of the args or the start of temps?
        if node.arguments.count > 0 {
          // Block has args so this is the end of arg marker
          step()
        }
        return
      }
      if let endBlock = currentToken, endBlock.type == .closeSquareParen {
        // close of block - no body!
        return
      }
      guard let blockArg = currentToken as? ValueToken, blockArg.type == .blockArg else {
        throw parserError(expecting: "block argument")
      }
      let variableNode = VariableNode(blockArg.value)
      node.arguments.append(variableNode)
      variableNode.parent = node
      step()
    }
  }

  func parseParenExpression() throws -> ExpressionNode? {
    guard let token = currentToken, token.type == .openParen else {
      return nil
    }
    step()
    let expression = try parseExpression()
    if let closeParen = currentToken, closeParen.type == .closeParen {
      step()
      return expression
    }
    throw parserError(expecting: "')'")
  }

  func parseLiteral() throws -> LiteralNode? {
    let savedPosition = position
    if let numberNode = parseLiteralNumber() {
      return numberNode
    }
    resetTo(savedPosition)
    if let symbolNode = parseLiteralSymbol() {
      return symbolNode
    }
    resetTo(savedPosition)
    if let characterNode = parseLiteralCharacter() {
      return characterNode
    }
    resetTo(savedPosition)
    if let stringNode = parseLiteralString() {
      return stringNode
    }
    resetTo(savedPosition)
    if let arrayNode = try parseLiteralArrayConstant() {
      return arrayNode
    }
    resetTo(savedPosition)
    if let byteArrayNode = try parseLiteralByteArrayConstant() {
      return byteArrayNode
    }
    resetTo(savedPosition)
    return nil
  }

  func parseLiteralNumber() -> LiteralNumberNode? {
    if let numberToken = currentToken as? ValueToken, numberToken.type == .number {
      step()
      return LiteralNumberNode(numberToken.value)
    }
    return nil
  }

  func parseLiteralSymbol() -> LiteralSymbolNode? {
    if let symbolToken = currentToken as? ValueToken, symbolToken.type == .symbol {
      step()
      return LiteralSymbolNode(symbolToken.value)
    }
    return nil
  }

  func parseLiteralCharacter() -> LiteralCharacterNode? {
    if let characterToken = currentToken as? ValueToken, characterToken.type == .character {
      step()
      return LiteralCharacterNode(characterToken.value)
    }
    return nil
  }

  func parseLiteralString() -> LiteralStringNode? {
    if let stringToken = currentToken as? ValueToken, stringToken.type == .string {
      step()
      return LiteralStringNode(stringToken.value)
    }
    return nil
  }

  func parseLiteralArrayConstant() throws -> LiteralArrayNode? {
    guard let token = currentToken, token.type == .openArray else {
      return nil
    }
    step()
    return try parseArray()
  }

  func parseLiteralByteArrayConstant() throws -> LiteralByteArrayNode? {
    guard let token = currentToken, token.type == .openByteArray else {
      return nil
    }
    step()
    return try parseByteArray()
  }

  func parseArray() throws -> LiteralArrayNode? {
    let node = LiteralArrayNode()
    while position < tokens.count {
      if currentToken!.type == .closeParen {
        step()
        break
      }
      if let literalNode = try parseLiteral() {
        node.values.append(literalNode)
        literalNode.parent = node
        continue
      }
      if let variableNode = parseIdentifier() {
        let symbolNode = LiteralSymbolNode("#"+variableNode.name)
        node.values.append(symbolNode)
        symbolNode.parent = node
        continue
      }
      if let token = currentToken, token.type == .openParen {
        step()
        let savedPosition = position
        if let arrayNode = try parseArray() {
          node.values.append(arrayNode)
          arrayNode.parent = node
          continue
        }
        resetTo(savedPosition)
      }
      throw parserError(expecting: "literal value")
    }
    return node
  }

  func parseDynamicArray() throws -> DynamicArrayNode? {
    guard let token = currentToken, token.type == .openCurlyParen else {
      return nil
    }
    step()
    let dynamicArrayNode = DynamicArrayNode()
    while position < tokens.count {
      if let endArray = currentToken, endArray.type == .closeCurlyParen {
        // end of array
        step()
        break
      }
      let savedPosition = position
      guard let statementNode = try parseStatement() else {
        resetTo(savedPosition)
        throw parserError(expecting: "statement")
      }
      dynamicArrayNode.values.append(statementNode)
      statementNode.parent = dynamicArrayNode
      if position >= tokens.count {
        break
      }
      if let endToken = currentToken, endToken.type == .period {
        step()
        continue
      }
      guard let endArray = currentToken, endArray.type == .closeCurlyParen else {
        throw parserError(expecting: "end of statement")
      }
    }
    return dynamicArrayNode
  }

  func isInteger(_ number: String) -> Bool {
    for c in number {
      if !c.isNumber {
        return false
      }
    }
    return true
  }

  func parseByteArray() throws -> LiteralByteArrayNode? {
    let node = LiteralByteArrayNode()
    while position < tokens.count {
      if currentToken!.type == .closeSquareParen {
        step()
        break
      }
      if let integerNode = currentToken as? ValueToken, integerNode.type == .number && isInteger(integerNode.value) {
        let numberNode = LiteralNumberNode(integerNode.value)
        node.values.append(numberNode)
        numberNode.parent = node
        step()
        continue
      }
      throw parserError(expecting: "byte value")
    }
    return node
  }

  func parseMessageExpression(receiver: ExpressionNode) throws -> ExpressionNode? {
    if position >= tokens.count {
      return receiver
    }
    // If that's the end of the expression then we just have the receiver
    if atEndOfExpression() {
      return receiver
    }
    let savedPosition = position
    if let unaryMessage = parseUnaryMessage(receiver: receiver) {
      return try parseMessageExpression(receiver: unaryMessage)
    }
    resetTo(savedPosition)
    if let binaryMessage = try parseBinaryMessage(receiver: receiver) {
      let binaryPosition = position
      if let keywordBinaryMessage = try parseKeywordMessage(receiver: binaryMessage) {
        return keywordBinaryMessage
      }
      resetTo(binaryPosition)
      return binaryMessage
    }
    resetTo(savedPosition)
    if let keywordMessage = try parseKeywordMessage(receiver: receiver) {
      return keywordMessage
    }
    resetTo(savedPosition)
    return nil
  }

  func parseUnaryMessage(receiver: ExpressionNode) -> ExpressionNode? {
    if position >= tokens.count {
      return receiver
    }
    // If that's the end of the expression then we just have the receiver
    if atEndOfExpression() {
      return receiver
    }
    guard let token = currentToken as? ValueToken, token.type == .identifier else {
      return nil
    }
    let selector = token.value
    let messageNode = MessageNode(receiver: receiver, selector: selector)
    receiver.parent = messageNode
    step()
    let savedPosition = position
    if let nextUnary = parseUnaryMessage(receiver: messageNode) {
      return nextUnary
    }
    resetTo(savedPosition)
    return messageNode
  }

  func parseBinaryMessage(receiver: ExpressionNode) throws -> ExpressionNode? {
    if position >= tokens.count {
      return receiver
    }
    // If that's the end of the expression then we just have the receiver
    if atEndOfExpression() {
      return receiver
    }
    guard let token = currentToken as? ValueToken, token.type == .binary else {
      return nil
    }
    let selector = token.value
    let messageNode = MessageNode(receiver: receiver, selector: selector)
    receiver.parent = messageNode
    step()
    // Arg must start with primary
    let primary = try parsePrimary()
    let savedPosition = position
    if let argument = parseUnaryMessage(receiver: primary) {
      argument.parent = messageNode
      messageNode.arguments = [argument]
    } else {
      resetTo(savedPosition)
      primary.parent = messageNode
      messageNode.arguments = [primary]
    }
    let binaryPosition = position
    if let nextBinary = try parseBinaryMessage(receiver: messageNode) {
      return nextBinary
    }
    resetTo(binaryPosition)
    return messageNode
  }

  func parseKeywordMessage(receiver: ExpressionNode) throws -> ExpressionNode? {
    if position >= tokens.count {
      return receiver
    }
    guard let token = currentToken as? ValueToken, token.type == .keyword else {
      return nil
    }
    var selector = ""
    var arguments: [ExpressionNode] = []
    var readingKeyword = true
    let keywordStart = position
    while position < tokens.count {
      if atEndOfExpression() {
        // End of keyword expression
        break
      }
      if readingKeyword {
        guard let keyword = currentToken as? ValueToken, keyword.type == .keyword else {
          throw parserError(expecting: "keyword")
        }
        selector.append(keyword.value)
        step()
        readingKeyword = false
        continue
      }
      // Arg must start with primary
      let primary = try parsePrimary()
      if position < tokens.count && tokens[position].type == .keyword {
        // Was just a primary arg
        arguments.append(primary)
        readingKeyword = true
        continue
      }
      // Could be unary, or binary arg
      let savedPosition = position
      if let unaryArg = parseUnaryMessage(receiver: primary) {
        if let binaryArg = try parseBinaryMessage(receiver: unaryArg) {
          arguments.append(binaryArg)
          readingKeyword = true
          continue
        }
        arguments.append(unaryArg)
        readingKeyword = true
        continue
      }
      resetTo(savedPosition)
      if let binaryArg = try parseBinaryMessage(receiver: primary) {
        arguments.append(binaryArg)
        readingKeyword = true
        continue
      }
      resetTo(savedPosition)
      throw parserError(expecting: "keyword")
    }
    if selector == "" {
      resetTo(keywordStart)
      throw parserError(expecting: "keyword")
    }
    let keywordNode = MessageNode(receiver: receiver, selector: selector)
    receiver.parent = keywordNode
    keywordNode.arguments = arguments
    arguments.forEach { arg in arg.parent = keywordNode }
    return keywordNode
  }

  func parserError(expecting expected: String) -> ParserError {
    if let errorToken = currentToken as? ValueToken {
      return ParserError.syntaxError(reason: "Syntax error, expecting \(expected) but got \(errorToken.value)", position: errorToken.position)
    }
    if let errorToken = currentToken {
      return ParserError.syntaxError(reason: "Syntax error, expecting \(expected) but got \(errorToken.type)", position: errorToken.position)
    }
    return ParserError.syntaxError(reason: "EOF while reading \(expected)", position: nil)
  }
}
