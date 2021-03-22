package react

import scala.scalajs.js
import org.scalajs.dom
import js.JSConverters._

object Helpers {

  private def noop: Any => Unit = (_: Any) => ()

  case class ElementFactory(tagName: String) {
    def apply(
        key: String = "",
        id: String = "",
        classes: Seq[String] = Seq(),
        onClick: dom.MouseEvent => Unit = noop
    )(
        children: ReactNode*
    ): ReactNode = {
      val props = js.Dictionary[js.Any]()
      if (key != "") props.addOne(("key", key))
      if (id != "") props.addOne(("id", id))
      if (classes.nonEmpty) props.addOne(("className", classes.mkString(" ")))
      if (onClick != noop) {
        val converted: js.Function1[_, _] = onClick
        props.addOne(("onClick", converted))
      }
      React.createElement(tagName, props, children: _*)
    }
  }

  val div: ElementFactory = ElementFactory("div")
  val span: ElementFactory = ElementFactory("span")
  val button: ElementFactory = ElementFactory("button")

  implicit def stringToReactNode(string: String): ReactNode = string.asInstanceOf[ReactNode]
  implicit def numberToReactNode(number: Int): ReactNode = number.asInstanceOf[ReactNode]
  implicit def booleanToReactNode(boolean: Boolean): ReactNode = boolean.asInstanceOf[ReactNode]
  implicit def stringOptionToReactNode(option: Option[String]): ReactNode =
    option match {
      case Some(value) => value.asInstanceOf[ReactNode]
      case None        => ().asInstanceOf[ReactNode]
    }

  implicit class OnComponent[T <: js.Object](function: T => ReactNode) {
    def |(props: T): ReactNode = React.createElement(function, props)
  }

  def useState[T](initial: () => T): (T, (T => T) => Unit) = {
    val result = React.useState(initial)
    val state = result(0).asInstanceOf[T]
    val setter = result(1).asInstanceOf[js.Function1[js.Function1[T, T], Unit]]
    (state, updater => setter(updater))
  }
  def useEffect(effect: () => () => Unit, dependencies: Any*): Unit = {
    React.useEffect(() => effect(), dependencies.toJSArray)
  }
  def useLayoutEffect(effect: () => () => Unit, dependencies: Any*): Unit = {
    React.useEffect(() => effect(), dependencies.toJSArray)
  }
}
