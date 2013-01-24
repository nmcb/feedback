package feedback.rna

case class Codon(val _1: Nucleotide, val _2: Nucleotide, val _3: Nucleotide) {

   val rna      = RNA(_1, _2, _3)
   val length   = 3
   val iterator = rna.iterator

   def isStartCodon = Codon.Start.equals(this)

   def isStopCodon = Codon.StopCodons.contains(this)

   def encodesAminoAcid = !isStopCodon

}

object Codon {

   val Start = Codon(A, U, G)

   object Stop {
      val Amber = Codon(U, A, G)
      val Occur = Codon(U, G, A)
      val Opal  = Codon(U, A, A)

      def apply(n1: Nucleotide, n2: Nucleotide, n3: Nucleotide): Codon = Codon(n1,n2,n3)

      def unapply(codon: Codon): Option[(Nucleotide,Nucleotide,Nucleotide)] = {
         if (codon.isStopCodon) Some(codon._1, codon._2, codon._3) else None
      }
   }

   import Stop._

   val StopCodons: Set[Codon] = Set(Amber, Occur, Opal)

   val StopCodonNames: Codon => String = Map(Amber -> "Amber", Occur -> "Occur", Opal -> "Opal")

   def fromRNA(rna: RNA) = Codon(rna(0), rna(1), rna(2))
}

