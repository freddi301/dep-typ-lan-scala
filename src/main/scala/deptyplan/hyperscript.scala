package deptyplan

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element

object hyperscript {
  abstract class GenericNode {
    def render(into: org.scalajs.dom.raw.Element)
  }
  case class ElementNode(
      tagName: String,
      onClick: dom.MouseEvent => Unit = noop,
      classes: Seq[String] = Seq(),
      children: Seq[GenericNode] = Seq()
  ) extends GenericNode {
    def render(into: org.scalajs.dom.raw.Element): Unit = {
      val element = document.createElement(tagName)
      if (onClick != noop) element.addEventListener("click", onClick)
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
  def noop: Any => Unit = (_: Any) => {}
  object implicits {
    implicit def createGenericNodeSeqFromString(
        text: String
    ): Seq[GenericNode] =
      Seq(TextNode(text))
    implicit def createGenericNodeSeqFromGenericNode(
        node: GenericNode
    ): Seq[GenericNode] =
      Seq(node)
  }
  object helpers {
    def div(
        onClick: dom.MouseEvent => Unit = noop,
        classes: Seq[String] = Seq(),
        children: Seq[GenericNode] = Seq()
    ): ElementNode = ElementNode("div", onClick, classes, children)
    val fragment: FragmentNode.type = FragmentNode
  }
}
