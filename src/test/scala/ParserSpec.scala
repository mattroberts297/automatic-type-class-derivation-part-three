import org.scalatest.{MustMatchers, FlatSpec}

case class SimpleArguments(alpha: String = "alpha", beta: Int, charlie: Boolean)

class ParserSpec extends FlatSpec with MustMatchers {
  "Parser::apply" must "derive a parser for SimpleArguments" in {
    val args = List("a", "1", "true")
    val parsed = Parser[SimpleArguments].parse(args) // Magic.
    parsed must be (SimpleArguments("a", 1, true))
  }
}
