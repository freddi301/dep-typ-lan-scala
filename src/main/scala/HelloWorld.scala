import scala.scalajs.js
import org.scalajs.dom.{console, document}
import react.ReactDOM
import react.ReactNode
import react.Helpers._

object HelloWorld {
  def main(args: Array[String]): Unit =
    ReactDOM.render(App(new App.Props()), document.getElementById("root"))
}

object App extends FC {
  class Props() extends js.Object
  def render(props: Props): ReactNode = {
    val (maxColumns, setMaxColumns) = useState(() => 40)
    div(classes = Seq("main"))(
      div(classes = Seq("left"))(),
      div(classes = Seq("center scroll-parent"))(
        div(classes = Seq("scroll-child"))(
          input(
            `type` = "number",
            value = maxColumns.toString,
            onChange = event => { val maxColumns = event.currentTarget.value.toInt; setMaxColumns(_ => maxColumns) }
          )()
          // FormattingSample.viewFormatted(maxColumns),
          //renderWriter(format(maxColumns))
        )
      ),
      div(classes = Seq("right"))()
    )
  }
  /*

  case class RichString(text: String) extends CharacterSequence {
    val length: Int = text.length
  }

  val formatting: Formatting[RichString] = new Formatting[RichString]

  def format(maxColumns: Int): formatting.Writer =
    formatting.Format(maxColumns = maxColumns, indentation = RichString("  ")).multiline(formatAbleSource)

  def renderWriter(writer: formatting.Writer): ReactNode =
    for ((line, lineIndex) <- writer.lines.zipWithIndex)
      yield div(key = lineIndex.toString)(
        for ((word, wordIndex) <- line.zipWithIndex)
          yield span(key = wordIndex.toString)(word.text)
      )

  def formatAbleSource: formatting.Token = {
    import Source._
    import formatting._
    def termToToken(term: Term, parens: Boolean): Token =
      term match {
        case Universe(level)       => One(RichString("type " + level))
        case Reference(identifier) => One(RichString(identifier))
        case Application(left, right) =>
          Many(
            Seq(
              termToToken(
                left,
                parens = left match {
                  case Application(_, _) => false
                  case _                 => true
                }
              ),
              termToToken(right, parens = true)
            ),
            RichString(" "),
            RichString(if (parens) "(" else ""),
            RichString(if (parens) ")" else ""),
            trailingSeparator = false
          )
        case Block(attributes) =>
          Many(
            (for ((attribute, entry) <- attributes)
              yield entry match {
                case Data(type_) =>
                  Seq(
                    Pair(
                      (RichString(attribute), termToToken(type_, parens = false)),
                      RichString(" : "),
                      RichString(""),
                      RichString(" ")
                    )
                  )
                case Parameter(type_) =>
                  Seq(
                    Pair(
                      (RichString(attribute), termToToken(type_, parens = false)),
                      RichString(" : "),
                      RichString(""),
                      RichString(" ")
                    )
                  )
                case Value(type_, value) =>
                  Seq(
                    Pair(
                      (RichString(attribute), termToToken(type_, parens = false)),
                      RichString(" : "),
                      RichString(""),
                      RichString(" ")
                    ),
                    Pair(
                      (RichString(attribute), termToToken(value, parens = false)),
                      RichString(" = "),
                      RichString(""),
                      RichString(" ")
                    )
                  )
              }).flatten.toSeq,
            RichString(", "),
            RichString("("),
            RichString(")"),
            isMulti = true
          )
        case Projection(block, attribute) =>
          Many(
            Seq(termToToken(block, parens = true), One(RichString(attribute))),
            RichString(" . "),
            RichString("("),
            RichString(")"),
            trailingSeparator = false
          )
        case Undefined() => One(RichString("?"))
        case Infer()     => One(RichString("_"))
      }
    termToToken(source, parens = false)
  }
   */

  val source: Unit = {
    import Source._
//    Block(
//      Map(
//        "boolean" -> Data(Universe(0)),
//        "true" -> Data(Reference("boolean")),
//        "false" -> Data(Reference("boolean"))
//        "not" -> Value(Pi("", Reference("boolean"), Reference("boolean")), Undefined()),
//        "and" -> Value(Pi("", Reference("boolean"), Pi("", Reference("boolean"), Reference("boolean"))), Undefined()),
//        "or" -> Value(Pi("", Reference("boolean"), Pi("", Reference("boolean"), Reference("boolean"))), Undefined()),
//        "nand" -> Value(
//          Infer(),
//          Lambda(
//            "x",
//            Infer(),
//            Infer(),
//            Lambda(
//              "y",
//              Infer(),
//              Infer(),
//              Application(Reference("not"), Application(Application(Reference("and"), Reference("x")), Reference("y")))
//            )
//          )
//        ),
//        "functor" -> Value(
//          Infer(),
//          Lambda(
//            "f",
//            Pi("", Universe(0), Universe(0)),
//            Infer(),
//            Interface(
//              Map(
//                "map" -> Pi(
//                  "",
//                  Pi("", Reference("a"), Reference("b")),
//                  Pi("", Application(Reference("f"), Reference("a")), Application(Reference("f"), Reference("b")))
//                ),
//                "flatMap" -> Pi(
//                  "",
//                  Pi("", Reference("a"), Application(Reference("f"), Reference("b"))),
//                  Pi("", Application(Reference("f"), Reference("a")), Application(Reference("f"), Reference("b")))
//                )
//              )
//            )
//          )
//        ),
//        "maybe" -> Data(Pi("", Universe(0), Universe(0))),
//        "none" -> Data(Application(Reference("maybe"), Reference("t"))),
//        "some" -> Data(Pi("", Reference("t"), Application(Reference("maybe"), Reference("t")))),
//        "maybe_functor" -> Value(
//          Application(Reference("functor"), Reference("maybe")),
//          Implementation(
//            Map(
//              "map" -> Undefined()
//            )
//          )
//        )
//      )
//    )
  }

}
