object exp {
  sealed trait BT
  case class Abs(body: BT) extends BT
  case class Var(index: Int) extends BT
  case class App(left: BT, right: BT) extends BT
  case class Sym(label: String) extends BT
  case class Far(uid: Int) extends BT
  case class Ctx(list: List[(Ctx, BT)]) {
    def read(index: Int): Option[(Ctx, BT)] = list.lift(index)
    def extend(term: BT): Ctx = Ctx((this, term) :: this.list)
  }

  var uid = 0
  def fresh: Int = { uid = uid + 1; uid }

  def eval(ctx: Ctx, term: BT): BT =
    term match {
      case App(left, right) =>
        eta(ctx, left) match {
          case Abs(body) => eta(ctx.extend(right), body)
          case left      => App(left, eta(ctx, right))
        }
      case Abs(body) =>
        val f = fresh
        Abs(replace(0, f, eta(ctx.extend(Far(f)), body)))
      case Var(index) =>
        ctx.read(index) match {
          case None                     => Sym("undefined")
          case Some((found_ctx, found)) => eta(found_ctx, found)
        }
      case _ => term
    }

  def replace(level: Int, far: Int, term: BT): BT =
    term match {
      case Far(uid) if (uid == far) => Var(level)
      case Abs(body)                => Abs(replace(level + 1, far, body))
      case App(left, right)         => App(replace(level, far, left), replace(level, far, right))
      case term                     => term
    }

  def eta(ctx: Ctx, term: BT): BT =
    term match {
      case Abs(App(term, Var(0))) if !uses(0, term) => eval(ctx, term)
      case term                                     => eval(ctx, term)
    }

  def uses(level: Int, term: BT): Boolean =
    term match {
      case Abs(body)        => uses(level + 1, body)
      case Var(index)       => index == level
      case App(left, right) => uses(level, left) || uses(level, right)
      case _                => false
    }

  def shouldEvaluate(input: BT, expected: BT): Either[BT, BT] = {
    val output = eta(Ctx(List()), input)
    if (output != expected) Left(output) else Right(output)
  }

  sealed trait T
  case class Universe(level: Int) extends T
  case class Pi(from: T, to: T) extends T
  case class Application(left: T, right: T) extends T
  case class Lambda(from: T, to: T) extends T

  def get_type(term: T): Option[T] =
    term match {
      case Universe(level) => Some(Universe(level + 1))
      case Pi(from, to) =>
        (get_type(from), get_type(to)) match {
          case (Some(Universe(from)), Some(Universe(to))) => Some(Universe(math.max(from, to)))
          case _                                          => None
        }
      case Application(left, right) => (get_type(left), get_type(right)) match {
        case (Some(Pi(from , to)), Some(right)) if from == right => Some(to)
        case _ => None
      }
      case Lambda(from, to) =>
        get_type(to) match {
          case None => None
          case Some(to) =>
            (get_type(from), get_type(to)) match {
              case (Some(Universe(_)), Some(Universe(_))) => Some(Pi(from, to))
            }
        }
    }

}
import exp.{App, Abs, Var, Sym, shouldEvaluate}
shouldEvaluate(App(Abs(Var(0)), Sym("dummy")), Sym("dummy"))
shouldEvaluate(App(Abs(Var(0)), Abs(Var(0))), Abs(Var(0)))
shouldEvaluate(Abs(App(Abs(Var(0)), Abs(Var(0)))), Abs(Abs(Var(0))))
shouldEvaluate(Abs(App(Abs(Var(0)), Var(0))), Abs(Var(0)))
shouldEvaluate(Abs(App(App(Abs(Abs(Var(1))), Var(0)), Sym("dummy"))), Abs(Var(0)))
shouldEvaluate(Abs(App(App(Abs(Abs(Var(2))), Sym("dummy")), Var(0))), Abs(Var(0)))
shouldEvaluate(Abs(App(Sym("dummy"), Var(0))), Sym("dummy"))