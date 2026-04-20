package io.joern.x2cpg.passes.pointsto

/** Framework-agnostic DI binding table. Maps interface full names to concrete implementation types that the DI
  * container wires at runtime. Consumed by [[ConstraintCollector]] to seed synthetic allocations at injection points.
  *
  * @param interfaceToImpls   interface/abstract type → concrete implementations bound at runtime
  * @param classRegistrations types instantiated reflectively by a framework (e.g. Jersey resources, Spring components)
  */
final case class DiBindings(
  interfaceToImpls: Map[String, Set[String]],
  classRegistrations: Set[String]
) {
  def isEmpty: Boolean = interfaceToImpls.isEmpty && classRegistrations.isEmpty

  def implsFor(typeFullName: String): Set[String] =
    interfaceToImpls.getOrElse(typeFullName, Set.empty)
}

object DiBindings {
  val empty: DiBindings = DiBindings(Map.empty, Set.empty)
}
