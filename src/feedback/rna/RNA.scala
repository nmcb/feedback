package feedback.rna

import collection.generic.CanBuildFrom
import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}


final class RNA private(val groups: Array[Int], val length: Int)
extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA] {

   // Import companion object definitions
   import RNA._

   // Mandatory sequencing implementation of ‘apply‘ in ‘IndexedSeq‘
   def apply(idx: Int): Base = {
      if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
      Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
   }

   /**
    * Optional:  re-implementation of foreach, making it more efficient
    * in speed.  We can mitigate against indirection  (necessary for
    * iteration, i.e. in the default implementation)  by utilizing the
    * indexing property of a feedback.rna.RNA sequence since we know the length of
    * the feedback.rna.RNA sequence and the group size N in terms of feedback.rna.Base symbols.
    **/
   override def foreach[U](f: Base => U): Unit = {
      var i = 0
      var b = 0
      while (i < length) {
         b = if (i % N == 0) {
            groups(i / N)
         }
         else {
            b >>> S
         }
         f(Base.fromInt(b & M))
         i += 1
      }
   }

   /**
    * Mandatory: re-implementation of ‘newBuilder‘ in ‘IndexedSeq‘ delegating to companion object builder factory.
    **/
   override protected[this] def newBuilder: Builder[Base, RNA] = RNA.newBuilder

}

object RNA {
   // number of bits in a group
   private val S = 2
   // bitmask to isolate a group
   private val M = (1 << S) - 1
   // number of groups in an Int
   private val N = 32 / S

   /**
    * Creates a RNA sequence from given scala collection sequence of bases.
    * @param buf The sequence of bases.
    * @return The RNA sequence from given sequence.
    */
   def fromSeq(buf: Seq[Base]): RNA = {
      val groups = new Array[Int]((buf.length + N - 1) / N)

      for (i <- 0 until buf.length) {
         groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)

      }

      new RNA(groups, buf.length)
   }


   def apply(bases: Base*) = fromSeq(bases)

   def newBuilder: Builder[Base, RNA] = new ArrayBuffer mapResult fromSeq

   implicit def canBuildFrom: CanBuildFrom[RNA, Base, RNA] = new CanBuildFrom[RNA, Base, RNA] {
      def apply(): Builder[Base, RNA] = newBuilder

      def apply(from: RNA): Builder[Base, RNA] = newBuilder
   }
}

