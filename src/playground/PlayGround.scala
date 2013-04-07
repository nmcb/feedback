package playground

import emc.rna._

object PlayGround extends App {

   val rna = RNA(C, C, C, C, C) ++
      Codon.Start.rna ++
      RNA(A, U, G, C, C, C) ++
      Codon.Stop.Amber.rna ++
      RNA(C, C, C) ++
      Codon.Stop.Opal.rna ++
      Codon.Start.rna ++
      RNA(C, A, U, G)

   def play() {
      println("rna: " + rna)
   }

   play()
}
