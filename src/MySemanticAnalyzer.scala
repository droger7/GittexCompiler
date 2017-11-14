import java.io._
import java.awt.Desktop
import java.io.{File, IOException}

class MySemanticAnalyzer() {

  var foundVar = new scala.collection.mutable.Stack[String] //Holds all defined variables from constants
  var varName = new scala.collection.mutable.Queue[String] //Holds the variable Names
  var varValue = new scala.collection.mutable.Queue[String] //Holds the variable Values
  var newStack = new scala.collection.mutable.Stack[String]
  val file = new PrintWriter(new File("Output.html"))
  var tempString: String = "" //Holds a temp string
  var currentScope : Int = 0 //Determines the scope of variables

  def checkIfWorks(): Unit = {

    while (!tempString.equalsIgnoreCase(CONSTANTS.DOCB) && Compiler.Parser.parseTree.nonEmpty) {
      tempString = Compiler.Parser.parseTree.pop()
      tempString match {
        case CONSTANTS.DOCB => newStack.push(tempString)
        case CONSTANTS.DOCE => newStack.push(tempString)
        case CONSTANTS.DEFB => varDef()
        case CONSTANTS.USEB => varUse()
        case _ => newStack.push(tempString)
      }
    }
    if (tempString.equalsIgnoreCase(CONSTANTS.DOCB) && !newStack.top.equalsIgnoreCase(CONSTANTS.DOCB)) {
      newStack.push(tempString)
    }
  }

  def varDef() = {
    foundVar.push(newStack.top)
    newStack.push(tempString)
  }
  def varUse() = {
    var varName: String = newStack.top
    newStack.push(tempString)
    while (!foundVar.contains(varName)) {
      if (Compiler.Parser.parseTree.isEmpty && !foundVar.contains(varName)) {
        println("Static semantic error. Variable '" + varName + "' is not defined")
        System.exit(1)
      }
      checkIfWorks()
    }
  }

  def convertToHTML() = {

    while (newStack.nonEmpty) {
      var spot: String = newStack.pop()
      spot match {

        case CONSTANTS.DOCB =>

          file.append("<html>\n")

        case CONSTANTS.DOCE =>

          file.append("\n</html>")

        case CONSTANTS.TITLEB =>

          file.append("<head>\n<title> ")
          spot = newStack.pop()
          while (!spot.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            file.append(spot + " ")
            spot = newStack.pop()
          }
          file.append(" </title>\n</head>\n")

        case CONSTANTS.HEADING =>

          file.append("<h1> ")
          spot = newStack.pop()
          while (!CONSTANTS.ALLCONSTANTS.contains(spot)) {
            file.append(spot + " ")
            spot = newStack.pop()
          }
          newStack.push(spot)
          file.append(" </h1>\n")

        case CONSTANTS.PARAB =>

          file.append("<p> ")
          currentScope = 1

        case CONSTANTS.PARAE =>

          file.append(" </p>\n")
          currentScope = 0

        case CONSTANTS.LINKB =>

          var linkText: String = ""
          var url: String = ""
          spot = newStack.pop()
          while (!spot.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            linkText = linkText + spot + " "
            spot = newStack.pop()
          }
          newStack.pop()
          url += newStack.pop()
          newStack.pop()
          file.append("<a href=\"" + url + "\">" + linkText + "</a> ")

        case CONSTANTS.LISTITEM =>

          file.append("\n<li> ")
          spot = newStack.pop()
          if (spot.contains("\n")) {
            file.append(spot + " </li>")
          }
          else if (spot.equalsIgnoreCase(CONSTANTS.USEB)) {
            newStack.push(spot) //Push back onto stack so it doesn't get skipped
          }
          else {
            if (!spot.equalsIgnoreCase(CONSTANTS.USEB)) {
              file.append(spot + " ")
              spot = newStack.top
              if (spot.contains("\n")) {
                spot = newStack.pop()
                file.append(spot + " </li>")
              }
            }
          }

        case CONSTANTS.NEWLINE =>

          file.append("<br>\n")

        case CONSTANTS.IMAGEB =>

          spot = newStack.pop()
          var linkText: String = ""
          var link: String = ""
          while (!spot.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
            linkText = linkText + spot + " "
            spot = newStack.pop()
          }
          newStack.pop()
          link += newStack.pop()
          newStack.pop()
          file.append("<img src=\"" + link + "\" alt=" + linkText + "\">")

        case CONSTANTS.BOLD =>

          var innerText: String = ""
          spot = newStack.pop()
          while (!spot.equalsIgnoreCase(CONSTANTS.BOLD)) {
            innerText += spot
            spot = newStack.pop()
          }
          file.append("<b> " + innerText + " </b>")

        case CONSTANTS.DEFB =>

          varName.enqueue(newStack.pop()) //Store name
          newStack.pop() //Ignore '='
          varValue.enqueue(newStack.pop()) //Store variable value
          newStack.pop() //Ignore ']'

        case CONSTANTS.USEB =>

          var useVar: String = newStack.pop()
          file.append(varValue(varName.indexOf(useVar, currentScope)) + " ") //Pulls var value
          newStack.pop() //Ignore ']'

        case _ => file.append(spot + " ")
      }

    }

    file.close()
    openHTMLFileInBrowser("Output.html")

  }


  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr: String) = {
    val file: File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }

  def semAnalyzer(): Unit = {
    checkIfWorks()
    convertToHTML()
  }
}
