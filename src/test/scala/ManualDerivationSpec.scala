import org.scalatest.{MustMatchers, FlatSpec}

class ManualDerivationSpec extends FlatSpec with MustMatchers {
  "Parser::apply" must "let us derive a parser for SimpleArguments" in {
    import Parser._
    import shapeless.Generic
    import shapeless.Lazy
    val args = List("a", "1", "true")
    val parser = Parser[SimpleArguments](
      genericParser(
        Generic[SimpleArguments],
        Lazy(hlistParser(
          Lazy(stringParser),
          hlistParser(
            Lazy(intParser),
            hlistParser(
              Lazy(boolParser),
              hnilParser
            )
          )
        ))
      )
    )
    val parsed = parser.parse(args)
    parsed must be (SimpleArguments("a", 1, true))
  }
}
