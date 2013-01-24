package feedback.rna

import collection.mutable.ListBuffer

object Ribosome {

   type PeptideChain = Seq[AminoAcid]

   /**
    * Decodes an RNA sequence into a sequence of peptide bounded molecules.  The
    * @param rna
    * @return
    */
   def decode(rna: RNA): Seq[PeptideChain] = {

      var started = false
      var sequence: ListBuffer[PeptideChain] = ListBuffer.empty
      var chain: ListBuffer[AminoAcid] = ListBuffer.empty

      for (codon <- rna.codons; if (started || codon.isStartCodon)) {

         if (codon.isStartCodon) started = true

         if (codon.isStopCodon && !chain.isEmpty) {
            sequence += chain.toSeq
            chain = ListBuffer.empty
         }

         if (codon.encodesAminoAcid) {
            chain += {
               AminoAcid.fromCodon(codon) match {
                  case Some(acid) => acid
                  case None => throw new Error("Could not decode into AminoAcid: " + codon)
               }
            }
         }
      }

      for (codon <- rna.codons) codon match {
         case Codon.Start => println("Start")
         case Codon.Stop(n1, n2, n3) => println("Stop(" + n1 + "," + n2 + "," + n3 + ")")
         case codon: Codon => println(codon)

      }

      sequence.toSeq
   }
}


object RibosomeTester extends App {

   def exec() {
      val rna = RNA(A, A, A, A, A, A) ++ Codon.Start.rna ++ RNA(A, C, G, U, G, U) ++ Codon.Stop.Amber.rna ++ RNA(C, U, G) ++ Codon.Stop.Opal.rna ++ RNA(G, U, U)
      println(Ribosome.decode(rna))
   }

   exec()
}


