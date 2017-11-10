class MyLexicalAnalyzer {

  var index : Int = -1  //Keeps track of character in fileContents
  var nextChar : Char = ' '  //Holds value of next char in fileContents
  var tokens : String = ""  //Holds value of possible token. Will go into currentToken if valid
  var reachedEnd : Boolean = false

  override def addChar(): Unit = { //Adds character to the potential token
    tokens = tokens + nextChar
  }

  override def getChar(): Unit = { //Gets next character from file input
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

  override def getNextToken(): Unit = { //Forms next token
    resetToken()
    getChar()
    nonSpace()

    if (reachedEnd == true) {}
    else if (CONSTANTS.SYMBOLS.contains(nextChar)) { //Special character state
      tokens = valid()
      tokens = tokens.map(_.toUpper)
      if (tokens.endsWith("\n")) {
        tokens = tokens.substring(0, tokens.length-1)
      }
      if (lookup(tokens)) {
        Compiler.currentToken = tokens
      }
      else {
        println("LEXICAL ERROR: Illegal token: '" + tokens + "' received")
        System.exit(1)
      }
    }
    else if (nextChar.isLetterOrDigit || nextChar.equals(':') || nextChar.equals('.') || nextChar.equals(',')) { //Text state
      addChar()
      tokens += textState()
      if (nextChar.equals(CONSTANTS.brackE) || nextChar.equals(CONSTANTS.parE) || nextChar.equals(CONSTANTS.equals) || nextChar.equals('\\')) {
        //Will decrement index so special characters aren't skipped
        index -= 1
      }
      Compiler.currentToken = tokens
    }
    else if (CONSTANTS.EOF.contains(nextChar)) {
      getNextToken() //Skip and get next token
    }
    else {
      println("LEXICAL ERROR: Illegal character: '" + nextChar + "' received")
      System.exit(1)
    }
  }

  def lookup(token : String): Boolean = { //Returns true if the token is legal
    return CONSTANTS.ALLCONSTANTS.contains(token.toUpperCase)
  }

  def valid() : String = { //Processes the annotation characters

    if (nextChar.equals(CONSTANTS.asterisk)) { // start '*'
      addChar()
      getChar()
      if (nextChar.equals(CONSTANTS.colon)) { //Special case for fixing skipping ':'
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
      if (nextChar.equals(CONSTANTS.brackB)) { //Will add ending bracket if required
        addChar()
      }
      if (tokens.equalsIgnoreCase(CONSTANTS.DOCE)) { //Ignores '\t' and '\n' after '\END' so program can quit
        nonSpace()
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
        println("Lexical error. Illegal character after '!'. Received: '" + nextChar + "'")
        System.exit(1)
      }
    }
    else if (nextChar.equals(CONSTANTS.brackE)) {   //The following functions add special characters
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


  def textState() : String = { //Reads in text until end of word, line or token
    var text : String = ""
    getChar()

    while (!nextChar.isSpaceChar && !(CONSTANTS.EOF contains nextChar) && !CONSTANTS.SYMBOLS.contains(nextChar) && !reachedEnd) {
      text += nextChar
      getChar()
    }
    if (nextChar.equals('\n')) {
      text += nextChar
    }
    if (nextChar.equals('\r')) {
      getChar()
      if (nextChar.equals('\n')) {
        text += nextChar
      }
    }

    return text
  }

  def nonSpace() : Unit = {   //Calls get char until a non space character is found
    while ((CONSTANTS.EOFSpace contains nextChar) && !reachedEnd) {
      getChar()
    }
  }
}
