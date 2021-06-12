object DeBrujin {
  sealed trait Term
  case class Atom(label: String) extends Term
  case class Universe(level: Int) extends Term
  case class Variable(index: Int) extends Term
  case class Application(left: Term, right: Term) extends Term
  case class Pi(label: String, from: Term, to: Term) extends Term
  case class Lambda(label: String, from: Term, to: Term) extends Term
  case class Let(label: String, left: Term, right: Term, in: Term) extends Term

  /** Modifies indices of the free variables by a given amount
    * @param by amount to add to free variables indices
    * @param depth how many lambdas were visited
    */
  def shift(by: Int, term: Term, depth: Int): Term = term match {
    case Atom(label) => Atom(label)
    case Universe(level) => Universe(level)
    case Variable(index) => val isFree = index >= depth; if (isFree) Variable(index + by) else Variable(index)
    case Application(left, right) => Application(shift(by, left, depth), shift(by, right, depth))
    case Pi(label, from, to) => Pi(label, shift(by, from, depth), shift(by, to, depth + 1))
    case Lambda(label, from, to) => Lambda(label, shift(by, from, depth), shift(by, to, depth + 1))
    case Let(label, left, right, in) =>
      Let(label, shift(by, left, depth), shift(by, right, depth), shift(by, in, depth + 1))
  }

  /** Replaces given index with a term, adjusting indices
    * @param ind index to be replaced
    * @param sub term that will replace the index
    * @param depth how many lambdas were visited
    */
  def replace(ind: Int, sub: Term, term: Term, depth: Int): Term = term match {
    case Atom(label) => Atom(label)
    case Universe(level) => Universe(level)
    case Variable(index) => if (index == ind) shift(depth, sub, 0) else Variable(index)
    case Application(left, right) => Application(replace(ind, sub, left, depth), replace(ind, sub, right, depth))
    case Pi(label, from, to) => Pi(label, replace(ind, sub, from, depth), replace(ind + 1, sub, to, depth + 1))
    case Lambda(label, from, to) => Lambda(label, replace(ind, sub, from, depth), replace(ind + 1, sub, to, depth + 1))
    case Let(label, left, right, in) =>
      Let(label, replace(ind, sub, left, depth), replace(ind, sub, right, depth), replace(ind + 1, sub, in, depth + 1))
  }

  case class Entry(label: String, typ: Term)
  case class Scope(scope: List[Entry]) {
    def extend(label: String, typ: Term): Scope = Scope(Entry(label, typ) :: scope)
    def lookup(index: Int): Option[Entry] = scope.lift(index)
  }

  def pretty_print(term: Term, scope: Scope, showParens: Boolean): String = {
    def parens(text: String): String = if (showParens) "(" + text + ")" else text
    term match {
      case Atom(label) => "'" + label
      case Universe(level) => parens("Type " + level)
      case Variable(index) =>
        scope.lookup(index) match {
          case Some(Entry(label, typ)) => index + "_" + label
          case None => index.toString
        }
      case Application(left, right) =>
        val showParens = left match {
          case Application(_, _) => false
          case _ => true
        }
        parens(pretty_print(left, scope, showParens = showParens) + " " + pretty_print(right, scope, showParens = true))
      case Pi(label, from, to) =>
        val showParens = to match {
          case Pi(_, _, _) => false
          case _ => true
        }
        "(" + label + " : " + pretty_print(from, scope, showParens = false) + ") -> " +
          pretty_print(to, scope.extend(label, from), showParens = showParens)
      case Lambda(label, from, to) =>
        val showParens = to match {
          case Lambda(_, _, _) => false
          case _ => true
        }
        "(" + label + " : " + pretty_print(from, scope, showParens = false) + ") => " +
          pretty_print(to, scope.extend(label, from), showParens = showParens)
      case Let(label, left, right, in) =>
        val showParens = in match {
          case Let(_, _, _, _) => false
          case _ => true
        }
        parens(
          label +
            " : " + pretty_print(left, scope, showParens = false) +
            " = " + pretty_print(right, scope, showParens = false) +
            "; " + pretty_print(in, scope, showParens = showParens)
        )
    }
  }

  def isSame(left: Term, right: Term): Boolean = (left, right) match {
    case (Atom(left), Atom(right)) => left == right
    case (Universe(left), Universe(right)) => left == right
    case (Variable(left), Variable(right)) => left == right
    case (Application(left_left, left_right), Application(right_left, right_right)) =>
      isSame(left_left, right_left) && isSame(left_right, right_right)
    case (Pi(_, left_from, left_to), Pi(_, right_from, right_to)) =>
      isSame(left_from, right_from) && isSame(left_to, right_to)
    case (Lambda(_, left_from, left_to), Lambda(_, right_from, right_to)) =>
      isSame(left_from, right_from) && isSame(left_to, right_to)
    case (Let(_, left_left, left_right, left_in), Let(_, right_left, right_right, right_in)) =>
      isSame(left_left, right_left) && isSame(left_right, right_right) && isSame(left_in, right_in)
    case _ => false
  }

  sealed trait Result
  case class Ok(term: Term) extends Result
  case class VariableNotInScope(index: Int, scope: Scope) extends Result
  case class Ko() extends Result

  def getType(term: Term, scope: Scope): Result = term match {
    case Atom(label) => Ok(Universe(0))
    case Universe(level) => Ok(Universe(level + 1))
    case Variable(index) =>
      scope.lookup(index) match {
        case Some(Entry(label, typ)) => Ok(typ)
        case None => VariableNotInScope(index, scope)
      }
    case Application(left, right) =>
      (getType(left, scope), getType(right, scope)) match {
        case (Ok(Pi(label, from, to)), Ok(right_type)) if isSame(from, right_type) =>
          Ok(shift(-1, replace(0, right, to, 1), 0))
        case _ => None
      }
    case Pi(label, from, to) =>
      (getType(from, scope), getType(to, scope.extend(label, from))) match {
        case (Some(Universe(from)), Some(Universe(to))) => Some(Universe(math.max(from, to)))
        case _ => None
      }
    case Lambda(label, from, to) =>
      (getType(from, scope), getType(to, scope.extend(label, from))) match {
        case (Some(Universe(_)), Some(to_type)) => Some(Pi(label + "_t", from, to_type))
        case _ => None
      }
    case Let(label, left, right, in) =>
      (getType(left, scope), getType(right, scope)) match {
        case (Some(Universe(_)), Some(right_type)) if isSame(left, right_type) =>
          getType(shift(-1, replace(0, right, in, 1), 0), scope.extend(label, left))
        case _ => None
      }
  }

  /*  def getValue(term: Term): Term = term match {
    case Universe(level) => Universe(level)
    case Pi(from, to) => Pi(from, to)
    case Application(left, right) =>
      getValue(left) match {
        case Lambda(from, to) => getValue(replace(right, to))
        case left => Application(left, right)
      }
    case Let(left, right, in) => getValue(replace(right, in))
    case Lambda(from, to) => Lambda(from, to)
  }

  def getNormal(term: Term): Term = term match {
    case Universe(level) => Universe(level)
    case Pi(from, to) => Pi(getNormal(from), getNormal(to))
    case Application(left, right) =>
      getValue(left) match {
        case Lambda(from, to) => getValue(replace(right, to))
        case left => Application(getNormal(left), getNormal(right))
      }
    case Let(left, right, in) => getNormal(replace(right, in))
    case Lambda(from, to) => Lambda(getNormal(from), getNormal(to))
  }*/

  def test: Unit = {
    case class Sample(source: Term) {
      val source_print: String = pretty_print(source, Scope(List()), showParens = false)
      val typ: Option[Term] = getType(source, Scope(List()))
      val typ_print: Option[String] = typ.map(pretty_print(_, Scope(List()), showParens = false))
    }
    val u1 = Sample(Pi("x", Universe(0), Universe(0)))
    val u2 = Sample(Pi("x", Universe(0), Pi("y", Universe(0), Variable(0))))
    val u3 = Sample(Lambda("x", Universe(0), Variable(0)))
    val u4 = Sample(
      Lambda(
        "equality",
        Pi("t", Universe(0), Pi("x", Variable(0), Pi("y", Variable(1), Universe(0)))),
        Lambda(
          "reflection",
          Pi(
            "t",
            Universe(0),
            Pi(
              "z",
              Variable(0),
              Application(Application(Application(Variable(2), Variable(1)), Variable(0)), Variable(0))
            )
          ),
          // Application(Variable(0), Atom("foo"))
          Atom("foo")
        )
      )
    )
    println("")
  }
}
