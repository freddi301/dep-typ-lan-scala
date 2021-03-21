import org.scalajs.dom.{console, document}
import react.Helpers._
import react.ReactDOM
import scala.scalajs.js
import scala.scalajs.js.timers.RawTimers.setInterval
import scala.scalajs.js.timers.RawTimers.clearInterval

object HelloWorld extends {
  def main(args: Array[String]): Unit = {
    ReactDOM.render(
      div(classes = Seq("main"))(
        div(classes = Seq("left"))(),
        div(classes = Seq("center"))(
          MyComponent | new MyComponent(name = "fred", surname = "bat"),
          div()(viewRaw(core.program): _*)
        ),
        div(classes = Seq("right"))()
      ),
      document.getElementById("root")
    )
  }

  class MyComponent(val name: String, val surname: String) extends js.Object
  val MyComponent = (props: MyComponent) => {
    val (counter, setCounter) = useState(() => 0)
    useEffect(() => {
      val intervalId = setInterval(() => setCounter(_ + 1), 1000)
      () => clearInterval(intervalId)
    })
    button(onClick = (event) => setCounter(_ + 1))(
      props.name,
      props.surname,
      counter,
      "                "
    )
  }

  def viewRaw(program: core.Program) =
    program.toSeq.map {
      case (identifier, definition) =>
        val mandatoryDesc =
          if (definition.mandatory.nonEmpty) " " + definition.mandatory.mkString(" ") else ""
        val typeDesc = definition.typ.map(typ => " : " + typ)
        val valueDesc = definition.value
          .map(value =>
            " = " + (value match {
              case core.Universe(level)                => "type"
              case core.Reference(identifier)          => identifier
              case core.Application(left, name, right) => left + "(" + name + " = " + right + ")"
            })
          )
        val inferredMandatory = core.infer_mandatory(identifier, Set()) -- definition.mandatory
        val inferredMandatoryDesc =
          if (inferredMandatory.nonEmpty) " " + inferredMandatory.mkString(" ") else ""
        div(key = identifier)(
          identifier,
          mandatoryDesc,
          span(classes = Seq("computed"))(inferredMandatoryDesc),
          typeDesc,
          valueDesc
        )
    }
}

object core {
  type Identifier = String
  sealed trait Rhs
  case class Universe(level: Int) extends Rhs
  case class Reference(identifier: Identifier) extends Rhs
  case class Application(left: Identifier, name: Identifier, right: Identifier) extends Rhs
  case class Definition(
      mandatory: Set[Identifier] = Set(),
      typ: Option[Identifier] = None,
      value: Option[Rhs] = None
  )
  type Program = Map[Identifier, Definition]

  def infer_mandatory(rhs: Rhs, skip: Set[Identifier]): Set[Identifier] =
    rhs match {
      case Universe(level) => Set()
      case Reference(identifier) =>
        (if (hasValue(identifier)) Set() else Set(identifier)) ++ infer_mandatory(
          identifier,
          skip
        )
      case Application(left, name, right) =>
        (if (hasValue(left)) Set() else Set(left)) ++ infer_mandatory(
          left,
          skip ++ Set(name)
        ) ++ (if (hasValue(right)) Set() else Set(right)) ++ infer_mandatory(
          right,
          skip
        )
    }
  def infer_mandatory(identifier: Identifier, skip: Set[Identifier]): Set[Identifier] = {
    if (skip.contains(identifier)) return Set()
    program.get(identifier) match {
      case None => Set()
      case Some(definition) =>
        val fromType: Set[Identifier] = definition.typ match {
          case None      => Set()
          case Some(typ) => Set(typ) ++ infer_mandatory(typ, skip ++ Set(identifier))
        }
        val fromValue: Set[Identifier] = definition.value match {
          case None        => Set()
          case Some(value) => infer_mandatory(value, skip ++ Set(identifier))
        }
        val fromMandatory =
          definition.mandatory.flatMap(infer_mandatory(_, skip ++ Set(identifier)))
        fromMandatory ++ fromType ++ fromValue
    }
  }
  def hasValue(identifier: Identifier): Boolean = program.get(identifier).flatMap(_.value).isDefined

  implicit class OnIdentifier(identifier: String) {
    def t(valueIdentifier: String): (String, Definition) =
      identifier -> Definition(typ = Some(valueIdentifier))
    def v(valueIdentifier: String): (String, Definition) =
      identifier -> Definition(value = Some(Reference(valueIdentifier)))
    def v(application: Application): (String, Definition) =
      identifier -> Definition(value = Some(application))
    def a(binding: (String, String)): Application = {
      val (name, right) = binding
      Application(identifier, name, right)
    }
    def req(mandatory: String*): OnReqs = new OnReqs(identifier, mandatory)
  }
  class OnReqs(identifier: String, mandatory: Seq[String]) {
    def t(valueIdentifier: String): (String, Definition) =
      identifier -> Definition(
        mandatory = mandatory.toSet,
        typ = Some(valueIdentifier)
      )
  }
  /*
    n: *
    z: n
    p: n
    s(p): n
    0 = z
    1 = s(p = 0)
    2 = s(p = 1)
    x : n
    inc = s(p = x)
   */
  val program: Program =
    Map(
      "type" -> Definition(value = Some(Universe(0))),
      "n".t("type"),
      "z".t("n"),
      "p".t("n"),
      "s".req("p").t("n"),
      "0".v("z"),
      "1".v("s".a("p" -> "0"))
      /*      "2".v("s".a("p" -> "1")),
      "x".t("n"),
      "inc".v("s".a("p" -> "x"))*/
    )
}
