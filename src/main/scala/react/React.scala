package react

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

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
