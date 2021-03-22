import scala.scalajs.js
import org.scalajs.dom.document
import scala.scalajs.js.timers.RawTimers.{clearInterval, setInterval}
import react.ReactDOM
import react.ReactNode
import react.Helpers._
import core.Source
import core.Source._
import core.SourceDSL._
import core.Program

object HelloWorld {

  def main(args: Array[String]): Unit = ReactDOM.render(app, document.getElementById("root"))

  def app: ReactNode =
    div(classes = Seq("main"))(
      div(classes = Seq("left"))(),
      div(classes = Seq("center"))(
        MyComponent | new MyComponent(name = "fred", surname = "bat"),
        viewRaw
      ),
      div(classes = Seq("right"))()
    )

  class MyComponent(val name: String, val surname: String) extends js.Object
  val MyComponent: MyComponent => ReactNode = props => {
    val (counter, setCounter) = useState(() => 0)
    useEffect(() => {
      val intervalId = setInterval(() => setCounter(_ + 1), 1000)
      () => clearInterval(intervalId)
    })
    button(onClick = _ => setCounter(_ + 1))(props.name, props.surname, counter)
  }

  def viewRaw: ReactNode =
    div()(
      program.terms.values
        .map(term =>
          div(key = term.identifier)(
            term.identifier,
            if (term.definition.required.nonEmpty) " " + term.definition.required.mkString(" ") else "",
            span(classes = Seq("computed"))(
              if (term.implicit_required.nonEmpty) " " + term.implicit_required.mkString(" ") else ""
            ),
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

  val program: Program = Program(
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
}
