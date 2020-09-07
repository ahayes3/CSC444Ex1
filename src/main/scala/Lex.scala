import scala.io.StdIn
import scala.util.matching.Regex
import java.io.IOException
class Lex {

  var token:Char = _
  var yytext: Char = _
  next()
  val LIT = 'L'

  @throws(classOf[ParseError])
  def next(): Unit = {
    try {
      if (token == Lex.EOL) {
        val c: Int = System.in.read()
        yytext = c.toChar
        if (c == -1) {
          token = Lex.EOL
          yytext = token
        }
        else if (
          yytext == Lex.AND || yytext == Lex.OR || yytext == Lex.XOR || yytext == Lex.EQ || yytext == Lex.QMARK || yytext == Lex.FALSE || yytext == Lex.TRUE || yytext == Lex.LP || yytext == Lex.RP
        )
          token = yytext
        else if (Lex.ID.matches(token.toString()))
          token = LIT
        else
          println("Unexpected character: " + yytext)
      }
    } catch {
      case e: IOException => throw new ParseError()
    }
  }

  def check(tok:Char):Char = {
    if(tok == token)
      yytext
    else
      0
  }

  def matchChar(tok:Char):Char = {
    val lexval = check(tok)
    if(lexval != 0)
      next()
    lexval
  }
}

object Lex {
  def ID: Regex = "[a-z]".r
  def AND: Char = '&'
  def OR: Char = '|'
  def XOR: Char = '^'
  def EQ: Char = '^'
  def QMARK: Char = '^'
  def FALSE: Char = '0'
  def TRUE: Char = '1'
  def LP: Char = '('
  def RP: Char = ')'
  def EOL: Char = '\n'
}