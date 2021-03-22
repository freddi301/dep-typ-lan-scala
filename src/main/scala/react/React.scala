package react

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom
import js.JSConverters._

@js.native
@JSGlobal
object React extends js.Object {
  def createElement(
      tagName: String,
      props: js.Dictionary[js.Any],
      children: ReactNode*
  ): ReactNode =
    js.native
  def createElement[T <: js.Object](
      component: js.Function1[T, ReactNode],
      props: T,
      children: ReactNode*
  ): ReactNode = js.native
  def useState[T](initial: js.Function0[T]): js.Array[Any] = js.native
  def useEffect(effect: js.Function0[js.Function0[Unit]], dependencies: js.Array[Any]): Unit =
    js.native
  def useLayoutEffect(effect: js.Function0[js.Function0[Unit]], dependencies: js.Array[Any]): Unit =
    js.native
}

class ReactNode extends js.Object

@js.native
@JSGlobal
object ReactDOM extends js.Object {
  def render(node: ReactNode, element: dom.Element): Unit = js.native
}

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
  implicit def nodeSeqToReactNode(seq: Seq[ReactNode]): ReactNode = seq.toJSArray.asInstanceOf[ReactNode]

  implicit class OnFunctionalComponent[T <: js.Object](function: T => ReactNode) {
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
