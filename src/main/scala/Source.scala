object Source {

  type Identifier = String
  sealed trait Term
  case class Universe(level: Int) extends Term
  case class Reference(identifier: Identifier) extends Term
  case class Pi(head: Identifier, from: Term, to: Term) extends Term
  case class Lambda(head: Identifier, from: Term, to: Term, body: Term) extends Term
  case class Application(left: Term, right: Term) extends Term
  case class Let(head: Identifier, `type`: Term, value: Term, in: Term) extends Term
  // record
  case class Interface(attributes: Map[Identifier, Term]) extends Term
  case class Implementation(attributes: Map[Identifier, Term]) extends Term
  case class Projection(record: Term, attribute: Identifier) extends Term
  // interactive
  case class Undefined() extends Term
  case class Hole(identifier: Identifier) extends Term
  case class Infer() extends Term
  // class and traits
  case class Attribute(public: Boolean)
  case class Trait() extends Term
  case class Class() extends Term

  // top level
  sealed trait Entry
  case class Data(`type`: Term) extends Entry
  case class Value(`type`: Term, value: Term) extends Entry
  case class Program(entries: Map[Identifier, Entry])

}

/*

functor = (
  f : ( argument: type , return : type ),
  map : (
    a : type ,
    b : type ,
    mapper : ( argument : a , return : b ) ,
    argument : f ( argument = a ) . return ,
    return : f ( argument = b ) . return ,
  )
)

monad = (
  m : ( argument : type , return : type ),
  m_functor : functor ( f = m ),
  flat_map : (
    a : type ,
    b : type ,
    mapper : ( argument : a , return : m ( argument = b ) . return ) ,
    argument : m ( argument = a ) . return ,
    return : m ( argument = b ) . return ,
  )
)

. map ( mapper = ( argument , return = not ( x = argument ) ) )

  map (a => not a)

 */
