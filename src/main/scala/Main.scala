import scala.util.matching.Regex
import scala.io.StdIn
object Main {
  def main(args: Array[String]) {
    println("Start")
    val lex = new Lex()
    val parser = new Parser(lex)
    var input = StdIn.readLine("# ")
    while(input!="exit") {
      parser.parse(input)
      input = StdIn.readLine("# ")
    }
  }
}
