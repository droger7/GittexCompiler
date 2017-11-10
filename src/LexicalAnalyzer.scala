
trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Unit
  def getNextToken() : Unit
  def lookup(token : String) : Boolean
}
