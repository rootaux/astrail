package io.joern.x2cpg.passes.pointsto

import scala.collection.mutable

/** Mutable set of allocation-site indices backed by a growable bitset. Each index maps to an entry in
  * [[AllocationSiteTable]]. The solver's hot path only needs union and diff operations.
  */
final class PointsToSet private (private val _bits: mutable.BitSet) {

  /** Raw bitset access for delta-propagation in the solver. */
  def bits: mutable.BitSet = _bits

  def isEmpty: Boolean          = _bits.isEmpty
  def nonEmpty: Boolean         = _bits.nonEmpty
  def size: Int                 = _bits.size
  def contains(i: Int): Boolean = _bits.contains(i)
  def iterator: Iterator[Int]   = _bits.iterator

  /** Add a single allocation site index. Returns true if the set changed. */
  def add(i: Int): Boolean = {
    val before = _bits.size
    _bits.addOne(i)
    _bits.size != before
  }

  /** Union `other` into this set. Returns true if this set changed. */
  def unionInPlace(other: PointsToSet): Boolean = {
    val before = _bits.size
    _bits |= other._bits
    _bits.size != before
  }

  /** Union a raw BitSet delta into this set. Returns true if the set changed. */
  def absorb(delta: mutable.BitSet): Boolean = {
    val before = _bits.size
    _bits |= delta
    _bits.size != before
  }

  /** `this \ other` as a fresh BitSet. */
  def diff(other: PointsToSet): mutable.BitSet = _bits &~ other._bits

  /** `this \ other` against a raw BitSet. */
  def diffBits(other: mutable.BitSet): mutable.BitSet = _bits &~ other

  def toImmutable: scala.collection.immutable.BitSet = _bits.toImmutable

  override def toString: String = _bits.mkString("{", ",", "}")
}

object PointsToSet {
  def empty: PointsToSet                   = new PointsToSet(mutable.BitSet.empty)
  def single(i: Int): PointsToSet          = { val s = empty; s.add(i); s }
  def from(xs: Iterable[Int]): PointsToSet = { val s = empty; xs.foreach(s.add); s }
}

/** Interns allocation-site descriptors (CPG node id + type full name) into dense int indices for [[PointsToSet]].
  * Tracks the concrete type of each site so the solver can map points-to sets back to dynamic types.
  */
final class AllocationSiteTable {
  private val idToIndex    = mutable.LinkedHashMap.empty[Long, Int]
  private val indexToType  = mutable.ArrayBuffer.empty[String]
  private val indexToNode  = mutable.ArrayBuffer.empty[Long]

  /** Get-or-create an index for the given allocation site. */
  def intern(nodeId: Long, typeFullName: String): Int = {
    idToIndex.getOrElseUpdate(
      nodeId, {
        val idx = indexToType.size
        indexToType.addOne(typeFullName)
        indexToNode.addOne(nodeId)
        idx
      }
    )
  }

  def typeOf(index: Int): String       = indexToType(index)
  def nodeIdOf(index: Int): Long       = indexToNode(index)
  def size: Int                        = indexToType.size
  def typesOf(set: PointsToSet): Set[String] =
    set.iterator.map(indexToType.apply).toSet
}
