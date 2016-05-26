package emc.rna

import collection.generic.CanBuildFrom
import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}

/**
 * A ribonucleic acid sequence, e.g. an RNA sequence of nucleotide molecules.
 */
final class RNA private(val slots: Array[Int], val length: Int)
extends IndexedSeq[Nucleotide] with IndexedSeqLike[Nucleotide, RNA] {

  /**
   * Import companion object attributes.
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
   * Mandatory: re-implementation of ‘newBuilder‘ in ‘IndexedSeq‘ delegating to
   * the companion object builder factory.
   **/
  override protected[this] def newBuilder: Builder[Nucleotide, RNA] = RNA.newBuilder

  /**
   * Optional:  re-implementation of foreach, making it more efficient in space.
   * We can mitigate against indirection  (necessary for iteration, i.e. in the
   * default implementation)  by utilizing the indexing property of an emc.rna.RNA
   * sequence since we know the length of the sequence and the group size N in
   * terms of emc.rna.Nucleotide symbols.
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
   * A codon iterator for this sequence starting at given reading frame, will
   * drop {{% SIZE}} remaining nucleotides.
   * @return A codon iterator for this sequence.
   */
  def codons(rf: Int): Iterator[Codon] = for {
    group <- drop(rf).grouped(Codon.GROUP_SIZE)
    if (group.size == Codon.GROUP_SIZE)
  } yield Codon.fromSeq(group)
}

object RNA {

  /**
   * Defines the number of bits in a slot, i.e the number of bit's needed to encode one nucleotide.
   */
  private val S = 2

  /**
   * Defines the bitmask to isolate the least significant slot, i.e. cuts of a nucleotide from an RNA sequence.
   */
  private val M = (1 << S) - 1

  /**
   * Defines the number of slots in an Int, i.e. the number of nucleotides that fit in an Int.
   */
  private val N = 32 / S

  /**
   * Creates an RNA sequence from given scala collection sequence of nucleotides.
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
   * Defines the advice for a Seq[Nucleotide] to RNA collection type,
   * i.e injected into creation pointcuts of scala's Seq iff it's inferred element type equals Nucleotide.
   * @return The collection's creation advice.
   */
  implicit def canBuildFrom: CanBuildFrom[RNA, Nucleotide, RNA] = new CanBuildFrom[RNA, Nucleotide, RNA] {
    def apply(): Builder[Nucleotide, RNA] = newBuilder

    def apply(from: RNA): Builder[Nucleotide, RNA] = newBuilder // Used to match the receiver type if not final.
  }
}

