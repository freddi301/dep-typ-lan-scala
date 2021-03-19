package deptyplan

object core {
  type Identifier = String
  case class Definition(
      label: String = "",
      left: Identifier = "",
      right: Map[Identifier, Identifier] = Map()
  )
  type Program = Map[Identifier, Definition]
  val program: Program = Map(
    "true" -> Definition(left = "onTrue"),
    "false" -> Definition(left = "onFalse"),
    "not" -> Definition(
      left = "p",
      right = Map(
        "onTrue" -> "false",
        "onFalse" -> "true"
      )
    ),
    "true_alias" -> Definition(left = "true"),
    "b1" -> Definition(left = "not", right = Map("p" -> "true")),
    "b2" -> Definition(left = "not", right = Map("p" -> "false")),
    "b3" -> Definition(left = "not", right = Map("p" -> "b1")),
    "mapit" -> Definition(left = "f", right = Map("a" -> "false")),
    "not1" -> Definition(left = "not", right = Map("p" -> "a")),
    "b4" -> Definition(left = "mapit", right = Map("f" -> "not1"))
  )
  abstract class Term
  case class Reference(identifier: Identifier) extends Term
  case class Application(left: Term, right: Map[Identifier, Term]) extends Term
  case class Abstraction(head: Set[Identifier], body: Term) extends Term
  def term_from_definition(identifier: Identifier): Term = {
    val definition = program.getOrElse(identifier, Definition())
    val variables = (definition.right.values ++ Seq(definition.left))
      .filter(v => !program.contains(v))
      .toSet
    val body =
      if (definition.right.isEmpty) Reference(definition.left)
      else
        Application(
          Reference(definition.left),
          definition.right.view.mapValues(v => Reference(v)).toMap
        )
    if (variables.isEmpty) return body
    Abstraction(variables, body)
  }
  val term_scope: Map[Identifier, Term] = program.map {
    case (id, _) => (id, term_from_definition(id))
  }
  def get_value(term: Term): Term = {
    term match {
      case Reference(identifier) =>
        term_scope.get(identifier) match {
          case Some(value) => get_value(value)
          case None        => term
        }
      case Abstraction(_, _) => term
      case Application(left, right) =>
        get_value(left) match {
          case Abstraction(head, body) =>
            get_value(replace_abstraction(right, Abstraction(head, body)))
          case left_value => Application(left_value, right)
        }
    }
  }
  def replace_abstraction(right: Map[Identifier, Term], abstraction: Abstraction): Term = {
    val head =
      abstraction.head.filter(identifier => !right.contains(identifier))
    val body = replace(right, abstraction.body)
    if (head.isEmpty) body else Abstraction(head, body)
  }
  def replace(replacements: Map[Identifier, Term], term: Term): Term = {
    if (replacements.isEmpty) return term
    term match {
      case Reference(identifier) => replacements.getOrElse(identifier, term)
      case Application(left, right) =>
        Application(
          replace(replacements, left),
          right.view.mapValues(v => replace(replacements, v)).toMap
        )
      case Abstraction(head, body) =>
        Abstraction(
          head,
          replace(
            replacements.filter { case (k, _) => !head.contains(k) },
            body
          )
        )
    }
  }
  def pretty_print(term: Term, parens: Boolean): String = {
    val pl = if (parens) "(" else ""
    val pr = if (parens) ")" else ""
    term match {
      case Abstraction(head, body) =>
        pl ++
          head.toSeq.map(identifier => identifier).mkString(", ") ++
          " => " ++
          pretty_print(body, parens = false) ++
          pr
      case Application(left, right) =>
        pl ++
          pretty_print(left, parens = true) ++
          "(" ++
          right.toSeq
            .map({
              case (identifier, term) => identifier ++ " = " ++ pretty_print(term, parens = false)
            })
            .mkString(", ") ++
          ")" ++
          pr
      case Reference(identifier) => identifier
    }
  }
}
