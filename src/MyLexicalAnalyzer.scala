class MyLexicalAnalyzer extends LexicalAnalyzer{

  var index : Int = -1  //Keeps track of character in fileContents
  var nextChar : Char = ' '  //Holds value of next char in fileContents
  var tokens : String = ""  //Holds value of possible token. Will go into currentToken if valid
  var reachedEnd : Boolean = false

  override def addChar(): Unit = { //Adds next character to the token
    tokens = tokens + nextChar
  }

  override def getChar(): Unit = { //Gets next character from file
    index +=1
    if (index < Compiler.fileContents.length) {
      nextChar = Compiler.fileContents.charAt(index)
    }
    else {
      reachedEnd = true
    }
  }

  def resetToken(): Unit = {
    tokens = ""
  }

  override def getNextToken(): Unit = {
    resetToken()
    getChar()
    nonSpace()

    if (reachedEnd) {}
    else if (CONSTANTS.SYMBOLS.contains(nextChar)) {
      tokens = valid()
      tokens = tokens.map(_.toUpper)
      if (tokens.endsWith("\n")) {
        tokens = tokens.substring(0, tokens.length-1)
      }
      if (lookup(tokens)) {
        Compiler.currentToken = tokens
      }
      else {
        error(nextChar)
      }
    }
    else if (nextChar.isLetterOrDigit || nextChar.equals(':') || nextChar.equals('.') || nextChar.equals(',')) { //Text state
      addChar()
      tokens += textState()
      if (nextChar.equals(CONSTANTS.brackE) || nextChar.equals(CONSTANTS.parE) || nextChar.equals(CONSTANTS.equals) || nextChar.equals(CONSTANTS.slash)) {
        index -= 1
      }
      Compiler.currentToken = tokens
    }
    else if (CONSTANTS.EOF.contains(nextChar)) {
      getNextToken() //Skip and get next token
    }
    else {
      error(nextChar)
    }
  }

  def lookup(token : String): Boolean = {
    CONSTANTS.ALLCONSTANTS.contains(token.toUpperCase)
  }

  def valid() : String = {

    if (nextChar.equals(CONSTANTS.asterisk)) {
      addChar()
      getChar()
      if (nextChar.equals(CONSTANTS.colon)) {
        index -= 1
      }
    }

    else if (nextChar.equals(CONSTANTS.plus)) {
      addChar()
      tokens += textState()
    }

    else if (nextChar.equals(CONSTANTS.slash)) {
      addChar()
      tokens += textState()
      if (nextChar.equals(CONSTANTS.brackB)) {
        addChar()
      }
      if (tokens.equalsIgnoreCase(CONSTANTS.DOCE)) {
        nonSpace()
        if (index - Compiler.fileContents.length != 0) { //Stuff after '\END'
          index -= 1
          getNextToken()
          println("Syntax error. Tokens were received after '" + CONSTANTS.DOCE + "'. Received: '" + Compiler.currentToken + "'")
          System.exit(1)
        }
      }
    }

    else if (nextChar.equals(CONSTANTS.pound)) {
      addChar()
      tokens += textState()
    }

    else if (nextChar.equals(CONSTANTS.exclamation)) {
      addChar()
      getChar()
      if (nextChar.equals(CONSTANTS.brackB)) {
        addChar()
      }
      else {
        error(nextChar)
      }
    }

    else if (nextChar.equals(CONSTANTS.brackE)) {
      addChar()
    }

    else if (nextChar.equals(CONSTANTS.brackB)) {
      addChar()
    }

    else if (nextChar.equals(CONSTANTS.parE)) {
      addChar()
    }

    else if (nextChar.equals(CONSTANTS.parB)) {
      addChar()
    }

    else if (nextChar.equals(CONSTANTS.equals)) {
      addChar()
    }
    tokens
  }


  def textState() : String = {
    var text : String = ""
    getChar()

    while (!nextChar.isSpaceChar && !(CONSTANTS.EOF contains nextChar) && !CONSTANTS.SYMBOLS.contains(nextChar) && !reachedEnd) {
      text += nextChar
      getChar()
    }
    if (nextChar.equals(CONSTANTS.newLine)) {
      text += nextChar
    }
    if (nextChar.equals(CONSTANTS.lineStart)) {
      getChar()
      if (nextChar.equals(CONSTANTS.newLine)) {
        text += nextChar
      }
    }

    text
  }

  def nonSpace() : Unit = {
    while ((CONSTANTS.EOFSpace contains nextChar) && !reachedEnd) {
      getChar()
    }
  }

  def error(expected: Char) : Unit = {
    println("Lexical error. Illegal character after '!'. Received: '" + expected + "'")
    System.exit(1)
  }
}
