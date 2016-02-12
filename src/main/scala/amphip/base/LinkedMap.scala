package amphip.base

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder

/**
 * Immutable map preserving insertion order
 */
class LinkedMap[K, +V] private (aVect: Vector[K], aMap: Map[K, V]) extends immutable.Map[K, V] with immutable.MapLike[K, V, LinkedMap[K, V]] {
  private[this] val theVect = aVect
  private[this] val theMap = aMap

  override def empty = LinkedMap.empty

  def get(key: K): Option[V] = theMap.get(key)

  def +[V1 >: V](kv: (K, V1)): LinkedMap[K, V1] = {
    val newMap = theMap + kv
    val newVect =
      if (newMap.size != theMap.size) {
        theVect :+ kv._1
      } else {
        theVect
      }
    new LinkedMap(newVect, newMap)
  }

  override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): LinkedMap[K, V1] = ((repr: LinkedMap[K, V1]) /: xs.seq)(_ + _)

  def -(key: K): LinkedMap[K, V] = {
    val newVect = theVect filter { _ != key }
    val newMap = theMap - key
    new LinkedMap[K, V](newVect, newMap)
  }

  def iterator: Iterator[(K, V)] = for (key <- theVect.iterator) yield key -> theMap(key)

  /** alternative version of `updated`, not great wrt type inference though ... */
  def updated[V1 >: V](key: K, f: V => V1): LinkedMap[K, V1] = {
    get(key) match {
      case Some(value) => this + ((key, f(value)))
      case None => this
    }
  }
}

object LinkedMap extends generic.ImmutableMapFactory[LinkedMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LinkedMap[A, B]] = new MapCanBuildFrom[A, B]
  override def empty[A, B]: LinkedMap[A, B] = EmptyLinkedMap.asInstanceOf[LinkedMap[A, B]]

  private object EmptyLinkedMap extends LinkedMap[Any, Nothing](Vector.empty[Nothing], Map.empty[Any, Nothing]) {}
}