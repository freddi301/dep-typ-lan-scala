import scala.scalajs.js
import org.scalajs.dom.{console, document}
import react.ReactDOM
import react.ReactNode
import react.Helpers._
import core.{Program, Source, TermFacade}
import core.Source._
import core.SourceDSL._

object HelloWorld {

  def main(args: Array[String]): Unit =
    ReactDOM.render(App.Component | new App.Props(), document.getElementById("root"))
}

object App {
  class Props() extends js.Object
  def Component: Props => ReactNode =
    props => {
      val (program, setProgram) = useState(() => sample)
      val (focused, setFocused) = useState(() => "")
      val (maxColumns, setMaxColumns) = useState(() => 40)
      div(classes = Seq("main"))(
        div(classes = Seq("left"))(),
        div(classes = Seq("center"))(
          viewRaw(sample),
          hr()(),
          viewCompact(program),
          hr()(),
          input(
            value = focused,
            onChange = event => { val text = event.currentTarget.value; setFocused(_ => text) }
          )(),
          viewRaw(sample.focus(focused)),
          hr()(),
          input(
            `type` = "number",
            value = maxColumns.toString,
            onChange = event => { val value = event.currentTarget.value; setMaxColumns(_ => value.toInt) }
          )(),
          viewFormatted(maxColumns)
        ),
        div(classes = Seq("right"))()
      )
    }

  def viewRaw(program: Program): ReactNode =
    Fragment()(
      program.terms.values
        .map(term =>
          div(key = term.identifier)(
            span(classes = Seq("computed"))(term.source_reference_count + " "),
            term.identifier,
            if (term.definition.required.nonEmpty) " (" + term.definition.required.mkString(" ") + ")" else "",
            if (term.implicit_required.nonEmpty) " {" + term.implicit_required.mkString(" ") + "}" else "",
            for (typ <- term.definition.typ) yield " : " + typ,
            for (value <- term.definition.value) yield " = " + (value match {
              case Universe(_)                    => "*"
              case Reference(identifier)          => identifier
              case Application(left, name, right) => left + "(" + name + " = " + right + ")"
            })
          )
        )
        .toSeq
    )

  def viewCompact(program: Program): ReactNode = {
    def viewRhsIdentifier(identifier: Identifier): ReactNode =
      program.terms.get(identifier) match {
        case None => identifier
        case Some(term) =>
          val should_inline = term.source_reference_count == 1
          if (should_inline) term.definition.value match {
            case None        => identifier
            case Some(value) => viewRhsValue(value)
          }
          else identifier
      }
    def viewRhsValue(value: RightHandSide): ReactNode =
      value match {
        case Universe(_)           => "*"
        case Reference(identifier) => viewRhsIdentifier(identifier)
        case Application(left, on, right) =>
          Fragment()(viewRhsIdentifier(left), "(", on, " = ", viewRhsIdentifier(right), ")")
      }
    def viewLhsIdentifier(identifier: Identifier, braces: (String, String)): ReactNode = {
      val (lb, rb) = braces
      (for (
        term <- program.terms.get(identifier);
        typ <- term.definition.typ if term.source_reference_count == 1
      ) yield Fragment()(lb, identifier, " : ", viewRhsIdentifier(typ), rb))
        .getOrElse(Fragment()(lb, identifier, rb))
    }

    Fragment()(
      program.terms.values
        .filter(term => term.source_reference_count != 1)
        .map(term =>
          div(key = term.identifier)(
            span(classes = Seq("computed"))(term.source_reference_count + " "),
            term.identifier,
            " ",
            if (term.definition.required.nonEmpty)
              Fragment()(
                term.definition.required.toSeq
                  .map(required => Fragment(key = required)(viewLhsIdentifier(required, ("(", ")")), " "))
              )
            else Fragment()(),
            if (term.implicit_required.nonEmpty)
              Fragment()(
                term.implicit_required.toSeq
                  .map(required => Fragment(key = required)(viewLhsIdentifier(required, ("{", "}")), " "))
              )
            else Fragment()(),
            for (typ <- term.definition.typ) yield Fragment()(": ", viewLhsIdentifier(typ, ("", ""))),
            for (value <- term.definition.value) yield Fragment()("= ", viewRhsValue(value))
          )
        )
        .toSeq
    )
  }

  val sample: Program = Program(
    Source(
      Map(
        "type".u(0),
        "n".t("type"),
        "z".t("n"),
        "p".t("n"),
        "s".req("p").t("n"),
        "0".v("z"),
        "1".v("s".a("p" -> "0")),
        "2".v("s".a("p" -> "1")),
        "x".t("n"),
        "inc".v("s".a("p" -> "x"))
      )
    )
  )

  def viewFormatted(maxColumns: Int): ReactNode =
    div()(
      formatted(maxColumns).lines.map(line => div()(line.map(word => span()(word.mkString))))
    )

  val formatting = new Formatting[Char]
  val formattingSample: formatting.Term = {
    import formatting.{Word, One, Many, Term}
    implicit def stringToWord(string: String): Term = One(string.toCharArray.toSeq)
    def app(terms: Term*): Term = Many(terms, ", ", "(", ")")

    app(
      app("a", "b", "c"),
      app("aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc"),
      app("a", "b", "c"),
      app("aaaaaaaaaa", "bbbbbbbbbb", "cccccccccc")
    )

  }
  def formatted(maxColumns: Int): formatting.Writer = formatting.LevelFormat(maxColumns, "  ")(formattingSample)
}

class Formatting[Character] {

  type Word = Seq[Character]

  sealed trait Term {
    val inlineLength: Int = this match {
      case One(word) => word.length
      case Many(terms, separator, open, close) =>
        open.length + terms.map(term => term.inlineLength + separator.length).sum + close.length
    }
  }
  case class One(word: Word) extends Term
  case class Many(terms: Seq[Term], separator: Word, open: Word, close: Word) extends Term

  case class Writer(lines: Seq[Seq[Word]], currentColumn: Int) {
    def write(word: Word): Writer =
      Writer(
        currentColumn = this.currentColumn + word.length,
        lines = lines.slice(0, lines.length - 1).appended(lines.last.appended(word))
      )
    def endLine(): Writer =
      Writer(currentColumn = 0, lines = lines.appended(Seq()))
  }
  object Writer {
    def empty: Writer = Writer(currentColumn = 0, lines = Seq(Seq()))
  }

  case class LevelFormat(maxColumns: Int, indentation: Word) {
    def formatInline(writer: Writer, term: Term): Writer =
      term match {
        case One(word) => writer.write(word)
        case Many(terms, separator, open, close) =>
          terms.foldLeft(writer.write(open))((w, i) => formatInline(w, i).write(separator)).write(close)
      }
    implicit class OnWriter(writer: Writer) {
      def indent(level: Int): Writer = Seq.fill(level)(indentation).foldLeft(writer)((w, i) => w.write(i))
    }
    def formatMultiline(writer: Writer, term: Term, level: Int): Writer =
      term match {
        case One(word) => writer.write(word)
        case Many(terms, separator, open, close) =>
          terms
            .foldLeft(writer.write(open))((w, i) => {
              format(w.endLine().indent(level), i, level).write(separator)
            })
            .endLine()
            .indent(level - 1)
            .write(close)
      }
    def format(writer: Writer, term: Term, level: Int): Writer = {
      term match {
        case One(word) => writer.write(word)
        case Many(terms, separator, open, close) =>
          terms
            .foldLeft(writer.write(open))((w, i) => {
              val remainingColumns = maxColumns - w.currentColumn
              if (i.inlineLength <= remainingColumns) formatInline(w, i).write(separator)
              else formatMultiline(w, i, level + 1).write(separator)
            })
            .write(close)
      }
    }
    def apply(term: Term): Writer = format(writer = Writer.empty, term = term, level = 0)
  }
}

/*

(a, b, c), (
  aaaaaaaaaaaaa,
  bbbbbbbbbbbbb,
  ccccccccccccc,
), (a, b, c), (
  aaaaaaaaaaaaa,
  bbbbbbbbbbbbb,
  ccccccccccccc,
),

 */
