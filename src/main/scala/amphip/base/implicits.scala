package amphip.base

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder

object implicits {
  /**
   * Extra members for seqs
   */
  implicit class SeqOpts[A, C <: Seq[A]](xs: C with SeqLike[A, C]) {

    final def groupByLinked[K](f: A => K)(implicit cbf: CanBuildFrom[C, A, C]): LinkedMap[K, C] = {

      def newBuilder: mutable.Builder[A, C] = cbf()

      val m = mutable.LinkedHashMap.empty[K, mutable.Builder[A, C]]
      for (elem <- xs) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, newBuilder)
        bldr += elem
      }
      val b = LinkedMap.newBuilder[K, C]
      for ((k, v) <- m)
        b += ((k, v.result))

      b.result
    }
  }
}