package io.joern.x2cpg.passes.pointsto

/** String-based pointer-variable identifiers for locals, fields, and return slots. The solver prefixes these with
  * an integer context key at lookup time.
  */
object PointerVar {

  /** Local or parameter of a method. */
  def local(methodFullName: String, name: String): String =
    s"L:$methodFullName:$name"

  /** Instance or static field of a type. */
  def field(typeFullName: String, fieldName: String): String =
    s"F:$typeFullName:$fieldName"

  /** Synthetic return-value slot of a method. */
  def ret(methodFullName: String): String =
    s"R:$methodFullName"

  /** Per-call temporary holding the return value of a specific call site. */
  def callResult(callNodeId: Long): String =
    s"C:$callNodeId"

  /** `this` receiver of a method. */
  def thisOf(methodFullName: String): String =
    local(methodFullName, "this")
}

/** Constraint IR emitted by [[ConstraintCollector]] and consumed by [[AndersenSolver]].
  * Constraints are grouped by enclosing method; the solver instantiates each method's template under one or more
  * contexts. All pointer-variable references are [[PointerVar]] strings — no CPG node references are retained.
  */
sealed trait Constraint

object Constraint {

  /** `dst ⊇ {allocIndex}` — result of an allocation expression (e.g. `new Foo()`). */
  final case class Alloc(dst: String, allocIndex: Int) extends Constraint

  /** `dst ⊇ src` — intra-method copy (assignment, parameter pass, return propagation). */
  final case class Copy(dst: String, src: String) extends Constraint

  /** `dst ⊇ *base.field` — field load, expanded lazily per concrete type in `pt(base)`. */
  final case class Load(dst: String, base: String, field: String) extends Constraint

  /** `*base.field ⊇ src` — field store. Dual of [[Load]]. */
  final case class Store(base: String, field: String, src: String) extends Constraint

  /** Statically-resolved call. The solver inherits the caller's context. */
  final case class StaticCall(
    callNodeId: Long,
    calleeFullName: String,
    argVars: Vector[String],
    callResultVar: String
  ) extends Constraint

  /** Virtual call resolved via the receiver's points-to set. The solver instantiates callees under a fresh
    * context keyed by the receiver allocation site (k=1 object sensitivity).
    */
  final case class VirtualCall(
    callNodeId: Long,
    receiver: String,
    methodName: String,
    signature: String,
    argVars: Vector[String],
    callResultVar: String
  ) extends Constraint
}
