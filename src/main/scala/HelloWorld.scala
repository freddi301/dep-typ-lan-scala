import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import scala.collection.immutable.HashMap
import h.createTextNodeFromString

object HelloWorld {
  def main(args: Array[String]): Unit = {
    document.body.appendChild(
      h(
        "button",
        onClick = (event) => println("yaaay"),
        classes = Array("main"),
        children = Array("ya")
      )
    )
  }
}

object h {
  def apply(
      tagName: String,
      onClick: dom.MouseEvent => Unit = noop,
      classes: Array[String] = Array(),
      children: Array[Element] = Array()
  ): Element = {
    val element = document.createElement(tagName)
    if ((onClick) != (noop)) element.addEventListener("click", onClick)
    for (child <- children) element.appendChild(child)
    for (class_ <- classes) element.classList.add(class_)
    element
  }
  implicit def createTextNodeFromString(text: String): Element =
    document.createTextNode(text).asInstanceOf[Element]
  def noop: Any => Unit = (a: Any) => {}
  def css(style: String) = style
}
