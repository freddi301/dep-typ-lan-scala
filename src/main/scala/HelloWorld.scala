import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import org.scalajs.dom.MouseEvent
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import hypescript.createGenericNodeFromString
import hypescript.h
import hypescript.Fragment

object HelloWorld {
  def main(args: Array[String]): Unit =
    h(
      "div",
      classes = Seq("main"),
      children = viewRaw
    ).render(document.body)
  def viewRaw =
    Fragment.from(for ((id, d) <- core.program.toSeq) yield {
      Fragment(
        h(
          "div",
          children = Fragment(id, " = ", d.left, "(", Fragment.from(for ((k, v) <- d.right.toSeq) yield k ++ " = " ++ v ++ ", "), ")")
        ),
        h("div", children = core.term_from_definition(id).toString)
      )
    })
}

object hypescript {
  abstract class GenericNode {
    def render(into: org.scalajs.dom.raw.Element)
  }
  case class ElementNode(
      tagName: String,
      onClick: dom.MouseEvent => Unit = noop,
      classes: Seq[String] = Seq(),
      children: GenericNode = Fragment()
  ) extends GenericNode {
    def render(into: org.scalajs.dom.raw.Element) = {
      val element = document.createElement(tagName)
      if ((onClick) != (noop)) element.addEventListener("click", onClick)
      for (class_ <- classes) element.classList.add(class_)
      children.render(element)
      into.appendChild(element)
    }
  }
  case class TextNode(text: String) extends GenericNode {
    def render(into: Element): Unit = into.appendChild(document.createTextNode(text))
  }
  case class Fragment(children: GenericNode*) extends GenericNode {
    def render(into: Element): Unit = for (child <- children) child.render(into)
  }
  object Fragment {
    def from(children: Seq[GenericNode]) = Fragment(children: _*)
  }

  implicit def createGenericNodeFromString(text: String): GenericNode =
    TextNode(text)
  def noop: Any => Unit = (a: Any) => {}
  val h = ElementNode
}

object core {
  type Identifier = String
  case class Definition(
      label: String = "",
      left: Identifier = "",
      right: HashMap[Identifier, Identifier] = HashMap()
  )
  type Program = HashMap[Identifier, Definition]
  val program: Program = HashMap(
    "true" -> Definition(label = "true", left = "onTrue"),
    "false" -> Definition(label = "false", left = "onFalse"),
    "not" -> Definition(
      label = "not",
      left = "p",
      right = HashMap(
        "onTrue" -> "false",
        "onFalse" -> "true"
      )
    )
  )
  abstract class Term
  case class Reference(idnetifier: Identifier) extends Term
  case class Application(left: Term, right: Map[Identifier, Term]) extends Term
  case class Abstraction(head: Set[Identifier], body: Term) extends Term
  def term_from_definition(identifier: Identifier) = {
    val definition = program.getOrElse(identifier, Definition())
    val variables = (definition.right.values ++ Seq(definition.left)).filter(v => !program.contains(v)).toSet
    val body = Reference(definition.left)
    Abstraction(variables, body)
  }
}
