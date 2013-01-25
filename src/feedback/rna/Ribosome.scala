package feedback.rna

import collection.mutable.ListBuffer

object Ribosome {

   type PeptideChain = Seq[AminoAcid]

   /**
    * Decodes an RNA sequence into a reading frame dependent sequences of peptide bounded amino acid molecules.
    * @param rna
    * @return
    */
   def decode(rna: RNA)(rf: Int): Seq[PeptideChain] = {

      var started = false
      var sequence: ListBuffer[PeptideChain] = ListBuffer.empty
      var chain: ListBuffer[AminoAcid] = ListBuffer.empty

      for (codon <- rna.drop(rf).codons) codon match {
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
      val seq = List(List(Methionine, Methionine, Proline), List(Proline), List(Methionine, Histidine))

      println("RNA.toString ok?            :" + (RNA(C, C, C, C, C, A, U, G, A, U, G, C, C, C, U, A, G, C, C, C, U, A, A, A, U, G, C, A, U, G) == rna) + "  [" + rna + "]")
      println("RNA.codons ok?              :" + (List(Codon(A, U, G), Codon(A, U, G), Codon(C, C, C), Codon(U, A, G), Codon(C, C, C), Codon(U, A, A), Codon(A, U, G), Codon(C, A, U)) == rna.codons.toList) + "  [" + rna.codons.toList + "]")
      println("Ribsome.decode(rna)(0) ok?  :" + (seq == Ribosome.decode(rna)(0)) + "  [" + Ribosome.decode(rna)(0) + "]")
      println("Ribsome.decode(rna)(1) ok?  :" + (seq == Ribosome.decode(rna)(1)) + "  [" + Ribosome.decode(rna)(1) + "]")
      println("Ribsome.decode(rna)(2) ok?  :" + (seq == Ribosome.decode(rna)(2)) + "  [" + Ribosome.decode(rna)(2) + "]")
   }

   exec()
}


