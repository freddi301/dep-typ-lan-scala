object DeBrujin {

  // https://en.wikipedia.org/wiki/De_Bruijn_index

  sealed trait Term
  case class Lambda(body: Term) extends Term
  case class Variable(index: Int) extends Term
  case class Application(left: Term, right: Term) extends Term

  /** Modifies indices of the free variables by a given amount
    * @param by amount to add to free variables indices
    * @param depth how many lambdas were visited
    */
  def shift(by: Int, term: Term, depth: Int): Term = term match {
    case Lambda(body) => Lambda(shift(by, body, depth + 1))
    case Variable(index) => val isFree = index >= depth; if (isFree) Variable(index + by) else Variable(index)
    case Application(left, right) => Application(shift(by, left, depth), shift(by, right, depth))
  }

  /** Replaces given index with a term, adjusting indices
    * @param ind index to be replaced
    * @param sub term that will replace the index
    * @param depth how many lambdas were visited
    */
  def replace(ind: Int, sub: Term, term: Term, depth: Int): Term = term match {
    case Lambda(body) => Lambda(replace(ind + 1, sub, body, depth + 1))
    case Variable(index) => if (index == ind) shift(depth, sub, 0) else Variable(index)
    case Application(left, right) => Application(replace(ind, sub, left, depth), replace(ind, sub, right, depth))
  }

  /** Reduces lazily */
  def getValue(term: Term): Term = term match {
    case Lambda(body) => Lambda(body)
    case Variable(index) => Variable(index)
    case Application(left, right) => getValue(left) match {
        case Lambda(body) => getValue(shift(-1, replace(0, right, body, 1), 0))
        case left => Application(left, right)
      }
  }

  /** Reduces to normal form */
  def getNormal(term: Term): Term = term match {
    case Lambda(body) => Lambda(getNormal(body))
    case Variable(index) => Variable(index)
    case Application(left, right) => getValue(left) match {
        case Lambda(body) => getNormal(shift(-1, replace(0, right, body, 1), 0))
        case left => Application(getNormal(left), getNormal(right))
      }
  }
}

import DeBrujin.{Application, Lambda, Variable, getNormal, getValue}
val u1 = Application(Lambda(Application(Lambda(Variable(0)), Variable(20))), Variable(40))
val u2 = getValue(u1)
val u3 = getNormal(u1)
