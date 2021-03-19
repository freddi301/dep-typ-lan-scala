import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import hypescript.implicits._
import hypescript.helpers._

object HelloWorld {
  def main(args: Array[String]): Unit =
    div(classes = Seq("main"), children = viewRaw).render(document.body)
  def viewRaw =
    for ((id, definition) <- core.program.toSeq) yield {
      fragment(
        div(children =
          id ++ " = " ++ definition.left ++ "(" ++ (
            for ((k, v) <- definition.right.toSeq) yield k ++ " = " ++ v
          ).mkString(", ") ++ ")"
        ),
        div(children = id ++ " = " + core.term_from_definition(id).toString),
        div(children =
          id ++ " = " + core.get_value(core.term_from_definition(id))
        )
      )
    }
}

object hypescript {
  abstract class GenericNode {
    def render(into: org.scalajs.dom.raw.Element)
  }
  case class ElementNode(
      tagName: String,
      onClick: dom.MouseEvent => Unit = noop,
      classes: Seq[String] = Seq(),
      children: Seq[GenericNode] = Seq()
  ) extends GenericNode {
    def render(into: org.scalajs.dom.raw.Element) = {
      val element = document.createElement(tagName)
      if ((onClick) != (noop)) element.addEventListener("click", onClick)
      for (class_ <- classes) element.classList.add(class_)
      for (child <- children) child.render(element)
      into.appendChild(element)
    }
  }
  case class TextNode(text: String) extends GenericNode {
    def render(into: Element): Unit =
      into.appendChild(document.createTextNode(text))
  }
  case class FragmentNode(children: GenericNode*) extends GenericNode {
    def render(into: Element): Unit =
      for (child <- children) child.render(into)
  }
  def noop: Any => Unit = (a: Any) => {}
  object implicits {
    implicit def createGenericNodeSeqFromString(
        text: String
    ): Seq[GenericNode] =
      Seq(TextNode(text))
  }
  object helpers {
    def div(
        onClick: dom.MouseEvent => Unit = noop,
        classes: Seq[String] = Seq(),
        children: Seq[GenericNode] = Seq()
    ) = ElementNode("div", onClick, classes, children)
    val fragment = FragmentNode
  }

}

object core {
  type Identifier = String
  case class Definition(
      label: String = "",
      left: Identifier = "",
      right: Map[Identifier, Identifier] = Map()
  )
  type Program = Map[Identifier, Definition]
  val program: Program = Map(
    "true" -> Definition(label = "true", left = "onTrue"),
    "false" -> Definition(label = "false", left = "onFalse"),
    "not" -> Definition(
      label = "not",
      left = "p",
      right = Map(
        "onTrue" -> "false",
        "onFalse" -> "true"
      )
    ),
    "true_alias" -> Definition(left = "true"),
    "b1" -> Definition(left = "not", right = Map("p" -> "true"))
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
      if (definition.right.size == 0) Reference(definition.left)
      else
        Application(
          Reference(definition.left),
          definition.right.view.mapValues(v => Reference(v)).toMap
        )
    if (variables.size == 0) return body
    Abstraction(variables, body)
  }
  val term_scope = program.map {
    case (id, definition) => (id, term_from_definition(id))
  }
  def get_value(term: Term): Term = {
    term match {
      case Reference(identifier)   => term_scope.getOrElse(identifier, term)
      case Abstraction(head, body) => term
      case Application(left, right) =>
        get_value(left) match {
          case Abstraction(head, body) =>
            get_value(replace_abstraction(right, Abstraction(head, body)))
          case left_value => Application(left_value, right)
        }
    }
  }
  def replace_abstraction(
      right: Map[Identifier, Term],
      abstraction: Abstraction
  ): Term = {
    val head =
      abstraction.head.filter(identifier => !right.contains(identifier))
    val body = replace(right, abstraction.body)
    if (head.size == 0) body else Abstraction(head, body)
  }
  def replace(replacements: Map[Identifier, Term], term: Term): Term = {
    if (replacements.size == 0) return term
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
}
