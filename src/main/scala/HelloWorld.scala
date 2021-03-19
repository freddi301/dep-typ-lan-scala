import deptyplan.core
import deptyplan.hyperscript
import org.scalajs.dom.{Element, document}
import deptyplan.hyperscript.implicits._
import deptyplan.hyperscript.helpers._

object HelloWorld extends {
  def main(args: Array[String]): Unit = update(state)
  def update(state: State): Unit = {
    this.state = state
    render().render(document.body)
  }
  case class State(counter: Int)
  var state: State = State(counter = 0)
  def render(): hyperscript.GenericNode =
    div(
      classes = Seq("main", "scroll-parent"),
      children = div(
        classes = Seq("scroll-child"),
        children = Seq(
          div(
            onClick = (event) => update(State(counter = state.counter + 1)),
            children = state.counter.toString
          )
        ) ++ viewRaw
      )
    )
  def viewRaw: Seq[hyperscript.GenericNode] =
    for ((id, definition) <- core.program.toSeq) yield {
      fragment(
        div(children =
          id ++ " = " ++ definition.left ++ "(" ++ (
            for ((k, v) <- definition.right.toSeq) yield k ++ " = " ++ v
          ).mkString(", ") ++ ")"
        ),
        div(children =
          "  " ++ id ++ " = " ++
            core.term_from_definition(id).toString
        ),
        div(children =
          "  " ++ id ++ " = " ++
            core.pretty_print(core.get_value(core.term_from_definition(id)), parens = false)
        )
      )
    }
}
