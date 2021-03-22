package core

import core.Source._

case class Program(source: Source) {
  def infer_mandatory(rhs: RightHandSide, skip: Set[Identifier]): Set[Identifier] =
    rhs match {
      case Universe(_) => Set()
      case Reference(identifier) =>
        (if (hasValue(identifier)) Set() else Set(identifier)) ++ infer_mandatory(identifier, skip)
      case Application(left, name, right) =>
        Set() ++
          (if (hasValue(left)) Set() else Set(left)) ++ infer_mandatory(left, skip ++ Set(name)) ++
          (if (hasValue(right)) Set() else Set(right)) ++ infer_mandatory(right, skip)
    }
  def infer_mandatory(identifier: Identifier, skip: Set[Identifier]): Set[Identifier] = {
    if (skip.contains(identifier)) return Set()
    source.definitions.get(identifier) match {
      case None => Set()
      case Some(definition) =>
        Set() ++
          definition.required.flatMap(infer_mandatory(_, skip ++ Set(identifier))) ++
          (definition.typ match {
            case None      => Set()
            case Some(typ) => Set(typ) ++ infer_mandatory(typ, skip ++ Set(identifier))
          }) ++
          (definition.value match {
            case None        => Set()
            case Some(value) => infer_mandatory(value, skip ++ Set(identifier))
          })

    }
  }
  def hasValue(identifier: Identifier): Boolean = source.definitions.get(identifier).flatMap(_.value).isDefined
}
