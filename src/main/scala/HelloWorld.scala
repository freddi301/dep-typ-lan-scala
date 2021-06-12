import scala.scalajs.js
import org.scalajs.dom.{console, document}
import react.ReactDOM
import react.ReactNode
import react.Helpers._

import scala.annotation.tailrec

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
          )(),
          // FormattingSample.viewFormatted(maxColumns),
          renderWriter(format(maxColumns))
        )
      ),
      div(classes = Seq("right"))()
    )
  }

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
    val TODO = One(RichString("???"))
    def termToToken(term: Term, parens: Boolean): Token =
      term match {
        case Universe(level)       => One(RichString("type"))
        case Reference(identifier) => One(RichString(identifier))
        case Pi(_, _, _) =>
          @tailrec
          def collect(collected: Seq[(Identifier, Term)], term: Term): (Seq[(Identifier, Term)], Term) =
            term match {
              case Pi(head, from, to) => collect(collected.appended((head, from)), to)
              case term               => (collected, term)
            }
          val (froms, to) = collect(Seq(), term)
          Many(
            froms
              .map({
                case (head, from) =>
                  if (head == "") termToToken(from, parens = true)
                  else
                    Pair(
                      (RichString(head), termToToken(from, parens = false)),
                      RichString(" : "),
                      RichString("("),
                      RichString(")")
                    )
              })
              .appended(termToToken(to, parens = false)),
            RichString(" -> "),
            RichString(if (parens) "(" else ""),
            RichString(if (parens) ")" else ""),
            trailingSeparator = false,
            isMulti = true
          )
        case Lambda(head, from, to, body) =>
          @tailrec
          def collect(collected: Seq[(Identifier, Term)], term: Term): (Seq[(Identifier, Term)], Term) =
            term match {
              case Lambda(head, from, to, body) => collect(collected.appended((head, from)), body)
              case term                         => (collected, term)
            }
          val (froms, body) = collect(Seq(), term)
          Many(
            froms
              .map({
                case (head, from) =>
                  from match {
                    case Infer() => One(RichString(head))
                    case _ =>
                      Pair(
                        (RichString(head), termToToken(from, parens = false)),
                        RichString(" : "),
                        RichString("("),
                        RichString(")")
                      )
                  }
              })
              .appended(termToToken(body, parens = false)),
            RichString(" => "),
            RichString(if (parens) "(" else ""),
            RichString(if (parens) ")" else ""),
            trailingSeparator = false,
            isMulti = true
          )
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
        case Let(head, type_, value, in) => TODO
        case Interface(attributes) =>
          Many(
            (for ((attribute, type_) <- attributes)
              yield Pair(
                (RichString(attribute), termToToken(type_, parens = false)),
                RichString(" : "),
                RichString(""),
                RichString(" ")
              )).toSeq,
            RichString("; "),
            RichString("{ "),
            RichString("}"),
            isMulti = true
          )
        case Implementation(attributes) =>
          Many(
            (for ((attribute, type_) <- attributes)
              yield Pair(
                (RichString(attribute), termToToken(type_, parens = false)),
                RichString(" = "),
                RichString(""),
                RichString(" ")
              )).toSeq,
            RichString(", "),
            RichString("{ "),
            RichString("}"),
            isMulti = true
          )
        case Projection(record, attribute) => TODO
        case Undefined()                   => One(RichString("?"))
        case Hole(identifier)              => One(RichString("?" + identifier))
        case Infer()                       => One(RichString("_"))
        // case Ascription(value, type_)      => TODO
      }
    Many(
      (for ((entry, definition) <- source.entries) yield (definition match {
        case Data(type_) =>
          Seq(
            Pair(
              (RichString(entry), termToToken(type_, parens = false)),
              RichString(" : "),
              RichString(""),
              RichString("")
            )
          )
        case Value(type_, value) =>
          Seq(
            Pair(
              (RichString(entry), termToToken(type_, parens = false)),
              RichString(" : "),
              RichString(""),
              RichString("")
            ),
            Pair(
              (RichString(entry), termToToken(value, parens = false)),
              RichString(" = "),
              RichString(""),
              RichString("")
            )
          )
      })).flatten.toSeq,
      RichString(""),
      RichString(""),
      RichString("")
    )
  }

  val source: Source.Program = {
    import Source._
    Program(
      Map(
        "boolean" -> Data(Universe(0)),
        "true" -> Data(Reference("boolean")),
        "false" -> Data(Reference("boolean")),
        "not" -> Value(Pi("", Reference("boolean"), Reference("boolean")), Undefined()),
        "and" -> Value(Pi("", Reference("boolean"), Pi("", Reference("boolean"), Reference("boolean"))), Undefined()),
        "or" -> Value(Pi("", Reference("boolean"), Pi("", Reference("boolean"), Reference("boolean"))), Undefined()),
        "nand" -> Value(
          Infer(),
          Lambda(
            "x",
            Infer(),
            Infer(),
            Lambda(
              "y",
              Infer(),
              Infer(),
              Application(Reference("not"), Application(Application(Reference("and"), Reference("x")), Reference("y")))
            )
          )
        ),
        "functor" -> Value(
          Infer(),
          Lambda(
            "f",
            Pi("", Universe(0), Universe(0)),
            Infer(),
            Interface(
              Map(
                "map" -> Pi(
                  "",
                  Pi("", Reference("a"), Reference("b")),
                  Pi("", Application(Reference("f"), Reference("a")), Application(Reference("f"), Reference("b")))
                ),
                "flatMap" -> Pi(
                  "",
                  Pi("", Reference("a"), Application(Reference("f"), Reference("b"))),
                  Pi("", Application(Reference("f"), Reference("a")), Application(Reference("f"), Reference("b")))
                )
              )
            )
          )
        ),
        "maybe" -> Data(Pi("", Universe(0), Universe(0))),
        "none" -> Data(Application(Reference("maybe"), Reference("t"))),
        "some" -> Data(Pi("", Reference("t"), Application(Reference("maybe"), Reference("t")))),
        "maybe_functor" -> Value(
          Application(Reference("functor"), Reference("maybe")),
          Implementation(
            Map(
              "map" -> Undefined()
            )
          )
        )
      )
    )
  }

}

/*




 */
