

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var parseTree = new scala.collection.mutable.Stack[String]

  //Language Start
  override def gittex() = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      pushNGet()
      variableDefine()
      title()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        parseTree.push(Compiler.currentToken)
      }
      else {
        error(CONSTANTS.DOCE)
      }
    }
    else {
      error(CONSTANTS.DOCB)
    }

  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      pushNGet()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        pushNGet()
      }
      else {
        error(CONSTANTS.BRACKETE)
      }
    }
    else {
      error(CONSTANTS.TITLEB)
    }

  }

  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else if (Compiler.Scanner.reachedEnd) {}
    else {
      innerText()
      body()
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      pushNGet()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }
      innerText()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        pushNGet()
      }
      else {
        error(CONSTANTS.PARAE)
      }
    }
    else {
      error(CONSTANTS.PARAB)
    }
  }

  override def innerText(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      innerText()
    }
    else if (Compiler.Scanner.reachedEnd){}
    else if (isText()) {
      pushNGet()
      innerText()
    }
  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
    }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      pushNGet()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQUALS)) {
        pushNGet()
        needText()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          pushNGet()
          variableDefine()
        }
        else {
          error(CONSTANTS.BRACKETE)
        }
      }
      else {
        error(CONSTANTS.EQUALS)
      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      pushNGet()
      needText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        pushNGet()
      }
      else {
        error(CONSTANTS.BRACKETE)
      }
    }
  }

  override def bold(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      pushNGet()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        pushNGet()
      }
      else {
        error(CONSTANTS.BOLD)
      }
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      pushNGet()
    }
  }

  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerItem()
    }
    else if (Compiler.Scanner.reachedEnd) {}
    else if (isText()) { //Text
      pushNGet()
      innerItem()
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      pushNGet()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        pushNGet()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          pushNGet()
          pushNGet()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            pushNGet()
          }
          else {
            error(CONSTANTS.ADDRESSE)
          }
        }
        else {
          error(CONSTANTS.ADDRESSB)
        }
      }
      else {
        error(CONSTANTS.BRACKETE)
      }
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      pushNGet()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        pushNGet()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          pushNGet()
          pushNGet()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            pushNGet()
          }
          else {
            error(CONSTANTS.ADDRESSE)
          }
        }
        else {
          error(CONSTANTS.ADDRESSB)
        }
      }
      else {
        error(CONSTANTS.BRACKETE)
      }
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      pushNGet()
    }
  }

  //determines if the currentToken is text
  def isText() : Boolean = {
    if (Compiler.currentToken.contains(CONSTANTS.colon) || Compiler.currentToken.contains(CONSTANTS.period) || Compiler.currentToken.contains(CONSTANTS.comma)) {
      return true
    }
    if (Compiler.currentToken.last == CONSTANTS.newLine ) {
      return Compiler.currentToken.length == (Compiler.currentToken.filter(_.isLetterOrDigit).length+1)
    }
    Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length
  }

  def needText(): Unit = {
    if (isText()) {
      pushNGet()
      needText()
    }
  }
  def error(expected: String) : Unit = {
    println("SYNTAX ERROR. Expected: '" + expected + "'. Received: '" + Compiler.currentToken + "'")
    System.exit(1)
  }
  def pushNGet(): Unit = {
    parseTree.push(Compiler.currentToken)
    Compiler.Scanner.getNextToken()
  }
}
