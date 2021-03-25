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
    val new_skip = skip ++ Set(identifier)
    source.definitions.get(identifier) match {
      case None => Set()
      case Some(definition) =>
        Set() ++
          definition.required.flatMap(required =>
            (if (skip.contains(required)) Set() else Set(required)) ++ infer_required(required, new_skip)
          ) ++
          (definition.typ match {
            case None      => Set()
            case Some(typ) => Set(typ) ++ infer_required(typ, new_skip)
          }) ++
          (definition.value match {
            case None        => Set()
            case Some(value) => infer_required(value, new_skip)
          })

    }
  }
  def hasValue(identifier: Identifier): Boolean = source.definitions.get(identifier).flatMap(_.value).isDefined

  def infer_used(rhs: RightHandSide, skip: Set[Identifier]): Set[Identifier] =
    rhs match {
      case Universe(level)              => Set()
      case Reference(identifier)        => infer_used(identifier, skip)
      case Application(left, on, right) => infer_used(left, skip) ++ infer_used(on, skip) ++ infer_used(right, skip)
    }

  def infer_used(identifier: Identifier, skip: Set[Identifier]): Set[Identifier] = {
    if (skip.contains(identifier)) return Set()
    val new_skip = skip ++ Set(identifier)
    Set(identifier) ++ (source.definitions.get(identifier) match {
      case None => Set()
      case Some(definition) =>
        Set() ++
          (for (required <- definition.required) yield infer_used(required, new_skip)).flatten ++
          (for (typ <- definition.typ) yield infer_used(typ, new_skip)).getOrElse(Set()) ++
          (for (value <- definition.value) yield infer_used(value, new_skip)).getOrElse(Set())
    })
  }

  def focus(identifier: Identifier): Program = {
    val used = infer_used(identifier, Set())
    Program(Source(source.definitions.filter { case (identifier, _) => used.contains(identifier) }))
  }

  val terms: Map[Identifier, TermFacade] = source.definitions.toIterable.map({
    case (identifier, definition) => (identifier, TermFacade(this, identifier, definition))
  })
}

case class TermFacade(program: Program, identifier: Identifier, definition: Definition) {
  val inferred_required: Set[Identifier] = program.infer_required(identifier, Set())
  val implicit_required: Set[Identifier] = inferred_required -- definition.required
  val source_reference_count: Int = program.source.infer_source_reference_count(identifier)
}
