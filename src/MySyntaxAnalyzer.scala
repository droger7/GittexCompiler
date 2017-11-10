

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var parseTree = new scala.collection.mutable.Stack[String]
  
  override def gittex() = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      title()
      variableDefine()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        parseTree.push(Compiler.currentToken)
      }
      else {
        println("SYNTAX ERROR. Expected: '" + CONSTANTS.DOCE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR. Expected '" + CONSTANTS.DOCB + "'. Received '" + Compiler.currentToken + "'")
      System.exit(1)
    }
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR. Expected '" + CONSTANTS.BRACKETE + "'. Received '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR. Expected: '" + CONSTANTS.TITLEB + "'. Received: '" + Compiler.currentToken + "'")
      System.exit(1)
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
    else if (Compiler.Scanner.reachedEnd == true) {}
    else {
      innerText()
      body()
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }
      innerText()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR. Expected: '" + CONSTANTS.PARAE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR. Expected: '" + CONSTANTS.PARAB + "'. Received: '" + Compiler.currentToken + "'")
      System.exit(1)
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
    else if (Compiler.Scanner.reachedEnd == true){}
    else if (isText()) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
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
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQUALS)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        needText()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          variableDefine()
        }
        else {
          println("Syntax error. Expected: '" + CONSTANTS.BRACKETE + "'. Received: '" + Compiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR. Illegal token. Expected: '" + CONSTANTS.EQUALS + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR. Illegal token. Expected: '" + CONSTANTS.BRACKETE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def bold(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("SYNTAX ERROR. Illegal token. Expected: '" + CONSTANTS.BOLD + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
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
    else if (Compiler.Scanner.reachedEnd == true) {}
    else if (isText()) { //Text
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else {
            println("SYNTAX ERROR. Expected: '" + CONSTANTS.ADDRESSE + "'. Received: '" + Compiler.currentToken + "'")
            System.exit(1)
          }
        }
        else {
          println("SYNTAX ERROR. Expected: '" + CONSTANTS.ADDRESSB + "'. Received: '" + Compiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("SYNTAX ERROR. Expected: '" + CONSTANTS.BRACKETE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
          else {
            println("SYNTAX ERROR. Expected: '" + CONSTANTS.ADDRESSE + "'. Received: '" + Compiler.currentToken + "'")
            System.exit(1)
          }
        }
        else {
          println("Syntax error. Expected: '" + CONSTANTS.ADDRESSB + "'. Received: '" + Compiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("Syntax error. Expected: '" + CONSTANTS.BRACKETE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  def isText() : Boolean = {
    if (Compiler.currentToken.contains(':') || Compiler.currentToken.contains('.') || Compiler.currentToken.contains(',')) {
      return true
    }
    if (Compiler.currentToken.last == '\n' ) {
      return Compiler.currentToken.length == (Compiler.currentToken.filter(_.isLetterOrDigit).length+1)
    }
    Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length
  }

  def needText(): Unit = {
    if (isText()) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      needText()
    }
    else if (Compiler.Scanner.reachedEnd == true) {}
  }
}
