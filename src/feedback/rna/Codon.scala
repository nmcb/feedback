package feedback.rna

case class Codon(val _1: Nucleotide, val _2: Nucleotide, val _3: Nucleotide) {

   val rna      = RNA(_1, _2, _3)
   val length   = 3
   val iterator = rna.iterator

   def isStartCodon = Codon.Start.equals(this)

   def isStopCodon = Codon.StopCodons.contains(this)

   def encodesAminoAcid = !isStopCodon

   def aminoAcid = AminoAcid.fromCodon(this).get

}

object Codon {

   /**
    * The encoding size of a codon in nucleotides.
    */
   val SIZE = 3

   val Start = Codon(A, U, G)

   /**
    * Defines a stop codon extractor for the codons: Amber, Occur and Opal, i.e. UAG, UGA and UAA.
    */
   object Stop {
      val Amber = Codon(U, A, G)
      val Occur = Codon(U, G, A)
      val Opal  = Codon(U, A, A)

      def apply(n1: Nucleotide, n2: Nucleotide, n3: Nucleotide): Codon = Codon(n1, n2, n3)

      def unapply(codon: Codon): Option[(Nucleotide, Nucleotide, Nucleotide)] = {
         if (codon.isStopCodon) Some(codon._1, codon._2, codon._3) else None
      }
   }

   import Stop._

   /**
    * Convenience definition of the set of stop codons.
    */
   val StopCodons: Set[Codon] = Set(Amber, Occur, Opal)

   /**
    * Convenience definition of the set of stop codon names.
    */
   val StopCodonNames: Codon => String = Map(Amber -> "Amber", Occur -> "Occur", Opal -> "Opal")

   /**
    * Constructs a codon from the first three nucleotides of given RNA sequence.
    * @param rna The RNA sequence to construct a codon from.
    * @return A codon representation of the first three nucleotides of given RNA sequence.
    * @throws IndexOutOfBoundsException If given RNA sequence does not contain enough nucleotides to construct a codon.
    */
   def fromRNA(rna: RNA) = Codon(rna(0), rna(1), rna(2))
}

object CodonTester extends App {

   def exec() {
      println(Codon.Start match {case Codon(A, U, G) => true case _ => false})
      println(Codon(A, U, G) match {case Codon.Start => true case _ => false})
      println(Codon.Start == Codon(A, U, G))
      println(Codon.Start.isStartCodon)
      println(Codon.Stop.Amber == Codon(U, A, G))
      println(Codon.Stop.Amber.isStopCodon)
      println(Codon.Stop.Occur == Codon(U, G, A))
      println(Codon.Stop.Occur.isStopCodon)
      println(Codon.Stop.Opal == Codon(U, A, A))
      println(Codon.Stop.Opal.isStopCodon)
   }

   exec()
}




