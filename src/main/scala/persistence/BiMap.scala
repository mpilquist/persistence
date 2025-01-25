package persistence

import scala.collection.immutable.SortedMap

trait BiMapOps[K, V, +CC[x, y] <: BiMap[x, y] & BiMapOps[x, y, CC, ?], +C <: BiMapOps[K, V, CC, C]]:
  type This <: CC[K, V]

  def updated(k: K, v: V): CC[K, V]

  inline def +(kv: (K, V)): CC[K, V] = updated(kv(0), kv(1))

  def -(k: K): CC[K, V]

  def iterator: Iterator[(K, V)]

trait BiMap[K, V] extends BiMapOps[K, V, BiMap, BiMap[K, V]]:
  def get(k: K): Option[V]
  def getR(v: V): Option[K]

object BiMap:

  private[persistence] trait MapLike[M[x, +y]]:
    extension [K, V](self: M[K, V])
      def get(k: K): Option[V]
      def updated(k: K, v: V): M[K, V]
      def +(kv: (K, V)): M[K, V] = updated(kv(0), kv(1))
      def -(k: K): M[K, V]
      def iterator: Iterator[(K, V)]

  private[persistence] trait AbstractMapLike[M[x, +y] <: Map[x, y]] extends MapLike[M]:
    extension [K, V](self: M[K, V])
      def get(k: K): Option[V] = self.get(k)
      def iterator: Iterator[(K, V)] = self.iterator

  private[persistence] given MapLike[Map] = new AbstractMapLike[Map]:
    extension [K, V](self: Map[K, V])
      def updated(k: K, v: V): Map[K, V] = self.updated(k, v)
      def -(k: K): Map[K, V] = self - k

  private[persistence] given MapLike[SortedMap] = new AbstractMapLike[SortedMap]:
    extension [K, V](self: SortedMap[K, V])
      def updated(k: K, v: V): SortedMap[K, V] = self.updated(k, v)
      def -(k: K): SortedMap[K, V] = self - k

  private[persistence] trait DualMaps[K, V, M[x, +y]: MapLike](entries: M[K, V], inverse: Map[V, K]) extends BiMap[K, V]:
    override def get(k: K): Option[V] = entries.get(k)
    override def getR(v: V): Option[K] = inverse.get(v)
    override def updated(k: K, v: V): This = make(
      inverse.get(v).fold(entries)(entries - _) + (k -> v),
      entries.get(k).fold(inverse)(inverse - _) + (v -> k)
    )
    override def -(k: K): This = make(
      entries - k,
      entries.get(k).fold(inverse)(inverse - _)
    )
    protected def make(entries: M[K, V], inverse: Map[V, K]): This

    override def iterator: Iterator[(K, V)] = entries.iterator


  private[persistence] final class Impl[K, V](entries: Map[K, V], inverse: Map[V, K]) extends DualMaps[K, V, Map](entries, inverse):
    override type This = Impl[K, V]
    protected def make(entries: Map[K, V], inverse: Map[V, K]) = new Impl(entries, inverse)

trait SortedBiMap[K: Ordering, V] extends BiMap[K, V] with BiMapOps[K, V, SortedBiMap, SortedBiMap[K, V]]

object SortedBiMap:
  def empty[K: Ordering, V]: SortedBiMap[K, V] =
    new Impl(SortedMap.empty, Map.empty)

  private[persistence] final class Impl[K: Ordering, V](
    entries: SortedMap[K, V], inverse: Map[V, K]
  ) extends BiMap.DualMaps[K, V, SortedMap](entries, inverse) with SortedBiMap[K, V]:
    override type This = Impl[K, V]
    protected def make(entries: SortedMap[K, V], inverse: Map[V, K]) = new Impl(entries, inverse)

