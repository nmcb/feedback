package emc.rna

sealed abstract class Nucleotide
case object G extends Nucleotide
case object A extends Nucleotide
case object U extends Nucleotide
case object C extends Nucleotide

object Nucleotide {
   val fromInt: Int => Nucleotide = Array(G, A, U, C)
   val toInt: Nucleotide => Int = Map(G -> 0, A -> 1, U -> 2, C -> 3)
}