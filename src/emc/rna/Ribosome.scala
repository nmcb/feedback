package emc.rna

import collection.mutable.ListBuffer

object Ribosome {

   type PeptideChain = Seq[AminoAcid]

   /**
    * Decodes an RNA sequence into a reading frame dependent sequences of peptide bounded amino acid molecules.
    * @param rna The RNA sequence.
    * @return A sequence containing the amino acid molecules encoded by the RNA sequence.
    */
   def decode(rna: RNA)(rf: Int): Seq[PeptideChain] = {

      var started = false
      var sequence: ListBuffer[PeptideChain] = ListBuffer.empty
      var chain: ListBuffer[AminoAcid] = ListBuffer.empty

      for (codon <- rna.codons(rf)) codon match {
         case Codon.Start => {
            chain += codon.aminoAcid
            started = true
         }
         case Codon.Stop(_, _, _) => {
            if (started) {
               sequence += chain.toSeq
               chain = ListBuffer.empty
            }
         }
         case codon: Codon => {
            if (started) {
               chain += codon.aminoAcid
            }
         }
      }

      if (!chain.isEmpty) sequence += chain.toSeq
      sequence.toSeq
   }
}


object RibosomeTester extends App {

   def exec() {
      val rna = RNA(C, C, C, C, C) ++ Codon.Start.rna ++ RNA(A, U, G, C, C, C) ++ Codon.Stop.Amber.rna ++ RNA(C, C, C) ++ Codon.Stop.Opal.rna ++ Codon.Start.rna ++ RNA(C, A, U, G)
      val seq_0 = List(List(Methionine))
      val seq_1 = List()
      val seq_2 = List(List(Methionine, Methionine, Proline), List(Proline), List(Methionine, Histidine))

      println("RNA.toString ok?            : "
                 + (RNA(C, C, C, C, C, A, U, G, A, U, G, C, C, C, U, A, G, C, C, C, U, A, A, A, U, G, C, A, U, G) == rna)
                 + " => " + rna)
      println("RNA.codons(0) ok?           : "
                 + (List(Codon(C, C, C),
                         Codon(C, C, A),
                         Codon(U, G, A),
                         Codon(U, G, C),
                         Codon(C, C, U),
                         Codon(A, G, C),
                         Codon(C, C, U),
                         Codon(A, A, A),
                         Codon(U, G, C),
                         Codon(A, U, G)) == rna.codons(0).toList)
                 + " => " + rna.codons(0).toList)
      println("RNA.codons(1) ok?           : "
                 + (List(Codon(C, C, C),
                         Codon(C, A, U),
                         Codon(G, A, U),
                         Codon(G, C, C),
                         Codon(C, U, A),
                         Codon(G, C, C),
                         Codon(C, U, A),
                         Codon(A, A, U),
                         Codon(G, C, A)) == rna.codons(1).toList)
                 + " => " + rna.codons(1).toList)
      println("RNA.codons(2) ok?           : "
                 + (List(Codon(C, C, C),
                         Codon(A, U, G),
                         Codon(A, U, G),
                         Codon(C, C, C),
                         Codon(U, A, G),
                         Codon(C, C, C),
                         Codon(U, A, A),
                         Codon(A, U, G),
                         Codon(C, A, U)) == rna.codons(2).toList)
                 + " => " + rna.codons(2).toList)
      println("Ribsome.decode(rna)(0) ok?  : "
                 + (seq_0 == Ribosome.decode(rna)(0))
                 + " => " + Ribosome.decode(rna)(0))
      println("Ribsome.decode(rna)(1) ok?  : "
                 + (seq_1 == Ribosome.decode(rna)(1))
                 + " => " + Ribosome.decode(rna)(1))
      println("Ribsome.decode(rna)(2) ok?  : "
                 + (seq_2 == Ribosome.decode(rna)(2))
                 + " => " + Ribosome.decode(rna)(2))
   }

   exec()
}


