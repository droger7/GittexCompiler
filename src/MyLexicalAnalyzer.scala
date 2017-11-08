class MyLexicalAnalyzer {
  override def addChar(): Unit = ???

  // override def lookup(): Boolean = ???

  override def getNextToken(): Unit = {
    val c  = getChar()
  }

  override def getChar(): Char = ???
}
