object CONSTANTS {
  val DOCB : String = "\\BEGIN"
  val DOCE : String = "\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE: String = "]"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val NEWLINE : String = "\\"
  val EMPTY : String = ""
  val BOLD : String = "*"
  val LISTITEM : String = "+"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQUALS : String = "="
  val USEB : String = "\\USE["
  val HEADING : String = "#"

  val asterisk : Char = '*'
  val newLine : Char = '\n'
  val lineStart : Char = '\r'
  val plus : Char = '+'
  val equals : Char = '='
  val slash : Char = '\\'
  val exclamation :Char = '!'
  val pound : Char = '#'
  val brackB : Char = '['
  val brackE : Char = ']'
  val parB : Char = '('
  val parE : Char = ')'
  val colon : Char = ':'
  val period : Char = '.'
  val comma : Char = ','

  val ALLCONSTANTS : Array[String] = Array(DOCB, DOCE, TITLEB, BRACKETE, PARAB, PARAE, NEWLINE, EMPTY, BOLD, LISTITEM, LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQUALS, USEB, HEADING)
  val SYMBOLS : Array[Char] = Array(asterisk, plus, equals, slash, exclamation, pound, brackB, brackE, parB, parE)
  val EOFSpace : Array[Char] = Array('\r', '\t', '\f', '\b', ' ', '\n')
  val EOF : Array[Char] = Array('\r', '\t', '\f', '\b', '\n')
}
