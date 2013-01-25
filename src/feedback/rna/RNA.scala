package feedback.rna

import collection.generic.CanBuildFrom
import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}

/**
 * A ribonucleic acid sequence, e.g. an RNA sequence of nucleotide molecules.
 */
final class RNA private(val slots: Array[Int], val length: Int)
   extends IndexedSeq[Nucleotide] with IndexedSeqLike[Nucleotide, RNA] {

   /**
    * Import companion object definitions .
    */

   import RNA._

   /**
    * Mandatory sequencing implementation of ‘apply‘ in ‘IndexedSeq‘
    */
   def apply(index: Int): Nucleotide = {
      if (index < 0 || length <= index) throw new IndexOutOfBoundsException
      Nucleotide.fromInt(slots(index / N) >> (index % N * S) & M)
   }

   /**
    * Mandatory: re-implementation of ‘newBuilder‘ in ‘IndexedSeq‘ delegating to companion object builder factory.
    **/
   override protected[this] def newBuilder: Builder[Nucleotide, RNA] = RNA.newBuilder

   /**
    * Optional:  re-implementation of foreach, making it more efficient
    * in speed.  We can mitigate against indirection  (necessary for
    * iteration, i.e. in the default implementation)  by utilizing the
    * indexing property of a feedback.rna.RNA sequence since we know the length of
    * the feedback.rna.RNA sequence and the group size N in terms of feedback.rna.Nucleotide symbols.
    */
   override def foreach[U](f: Nucleotide => U): Unit = {
      var i = 0
      var b = 0
      while (i < length) {
         b = if (i % N == 0) slots(i / N) else b >>> S
         f(Nucleotide.fromInt(b & M))
         i += 1
      }
   }

   /**
    * Returns a codon iterator for this RNA sequence.  Includes the start codon, may drop 1 or 2 trailing nucleotides.
    * @return A codon iterator for this sequence.
    */
   //   def codons: Iterator[Codon] = for (triplet <- RNA.this.dropRight(length % 3).grouped(3)) yield Codon.fromRNA(triplet)
   def codons: Iterator[Codon] = {
      if (containsSlice(Codon.Start.rna)) {
         val start = indexOfSlice(Codon.Start.rna)
         for (triple <- drop(start).grouped(Codon.SIZE); if (triple.size == Codon.SIZE)) yield Codon.fromRNA(triple)
      }
      else {
         Iterator.empty
      }
   }
}

object RNA {

   /**
    * Defines the number of bits in a group
    */
   private val S = 2

   /**
    * Defines the bitmask to isolate a group
    */
   private val M = (1 << S) - 1

   /**
    * Defines the number of groups in an Int

    */
   private val N = 32 / S

   /**
    * Creates a RNA sequence from given scala collection sequence of nucleotides.
    * @param nucleotides The sequence of nucleotides.
    * @return The RNA sequence from given sequence.
    */
   def fromSeq(nucleotides: Seq[Nucleotide]): RNA = {
      val slots = new Array[Int]((nucleotides.length + N - 1) / N)
      for (i <- 0 until nucleotides.length) slots(i / N) |= Nucleotide.toInt(nucleotides(i)) << (i % N * S)
      new RNA(slots, nucleotides.length)
   }


   /**
    * Defines the collection constructor.
    * @param nucleotides An array of nucleotides to create a RNA sequence for.
    * @return The created RNA sequence.
    */
   def apply(nucleotides: Nucleotide*) = fromSeq(nucleotides)

   /**
    * Defines the pattern match injection point.
    * @param rna The RNA sequence to inject.
    * @return The matched rna sequence.
    */
   def unapplySeq(rna: RNA): Option[Seq[Nucleotide]] = Some(rna)

   /**
    * Creates a new RNA sequence builder function.
    * @return The RNA sequence builder.
    */
   def newBuilder: Builder[Nucleotide, RNA] = new ArrayBuffer mapResult fromSeq

   /**
    * Defines the injection pointcut for the RNA collection type, i.e merged during creation of a scala collection.
    * @return The collection's creation pointcut.
    */
   implicit def canBuildFrom: CanBuildFrom[RNA, Nucleotide, RNA] = new CanBuildFrom[RNA, Nucleotide, RNA] {
      def apply(): Builder[Nucleotide, RNA] = newBuilder

      def apply(from: RNA): Builder[Nucleotide, RNA] = newBuilder // Used to match the receiver type if not final.
   }
}

