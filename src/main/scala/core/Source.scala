package core

import Source._

object Source {

  type Identifier = String

  sealed trait RightHandSide
  case class Universe(level: Int) extends RightHandSide
  case class Reference(identifier: Identifier) extends RightHandSide
  case class Application(left: Identifier, on: Identifier, right: Identifier) extends RightHandSide

  case class Definition(
      required: Set[Identifier] = Set(),
      typ: Option[Identifier] = None,
      value: Option[RightHandSide] = None
  )

}

case class Source(definitions: Map[Identifier, Definition]) {

  val all_identifiers: Set[Identifier] = definitions.toIterable.flatMap {
    case (identifier, definition) =>
      Set(identifier) ++
        definition.required ++
        definition.typ.map(Set(_)).getOrElse(Set()) ++ definition.value
        .map({
          case Universe(_)                  => Set()
          case Reference(identifier)        => Set(identifier)
          case Application(left, on, right) => Set(left, on, right)
        })
        .getOrElse(Set())
  }.toSet

  def infer_source_reference_count(identifier: Identifier): Int =
    definitions.values.foldLeft(0)((memo, definition) =>
      memo +
        (if (definition.required.contains(identifier)) 1 else 0) +
        (if (definition.typ.contains(identifier)) 1 else 0) +
        definition.value
          .map({
            case Universe(0)  => 0
            case Reference(i) => if (i == identifier) 1 else 0
            case Application(left, _, right) =>
              (if (left == identifier) 1 else 0) + (if (right == identifier) 1 else 0)
          })
          .getOrElse(0)
    )

}

object SourceDSL {
  implicit class OnIdentifier(identifier: String) {
    def u(level: Int): (String, Definition) =
      identifier -> Definition(value = Some(Universe(level)))
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
    def req(mandatory: String*): OnRequired =
      OnRequired(identifier, mandatory)
  }
  case class OnRequired(identifier: String, mandatory: Seq[String]) {
    def t(valueIdentifier: String): (String, Definition) =
      identifier -> Definition(required = mandatory.toSet, typ = Some(valueIdentifier))
  }
}
