package core

import core.Source._

case class Program(source: Source) {
  def infer_required(rhs: RightHandSide, skip: Set[Identifier]): Set[Identifier] =
    rhs match {
      case Universe(_) => Set()
      case Reference(identifier) =>
        (if (hasValue(identifier)) Set() else Set(identifier)) ++ infer_required(identifier, skip)
      case Application(left, name, right) =>
        Set() ++
          (if (hasValue(left)) Set() else Set(left)) ++ infer_required(left, skip ++ Set(name)) ++
          (if (hasValue(right)) Set() else Set(right)) ++ infer_required(right, skip)
    }
  def infer_required(identifier: Identifier, skip: Set[Identifier]): Set[Identifier] = {
    if (skip.contains(identifier)) return Set()
    source.definitions.get(identifier) match {
      case None => Set()
      case Some(definition) =>
        Set() ++
          definition.required.flatMap(infer_required(_, skip ++ Set(identifier))) ++
          (definition.typ match {
            case None      => Set()
            case Some(typ) => Set(typ) ++ infer_required(typ, skip ++ Set(identifier))
          }) ++
          (definition.value match {
            case None        => Set()
            case Some(value) => infer_required(value, skip ++ Set(identifier))
          })

    }
  }
  def hasValue(identifier: Identifier): Boolean = source.definitions.get(identifier).flatMap(_.value).isDefined

  case class TermFacade(identifier: Identifier, definition: Definition) {
    val inferred_required: Set[Identifier] = infer_required(identifier, Set())
    val implicit_required: Set[Identifier] = inferred_required -- definition.required
  }
  val terms: Map[Identifier, TermFacade] = source.definitions.view
    .map({ case (identifier, definition) => (identifier, TermFacade(identifier, definition)) })
    .toMap
}
