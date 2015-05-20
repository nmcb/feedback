package emc.rna

sealed trait AminoAcid
case object Phenylalanine extends AminoAcid
case object Leucine extends AminoAcid
case object Isoleucine extends AminoAcid
case object Methionine extends AminoAcid
case object Valine extends AminoAcid
case object Serine extends AminoAcid
case object Proline extends AminoAcid
case object Threonine extends AminoAcid
case object Alanine extends AminoAcid
case object Tyrosine extends AminoAcid
case object Histidine extends AminoAcid
case object Glutamine extends AminoAcid
case object Asparagine extends AminoAcid
case object Lysine extends AminoAcid
case object AsparticAcid extends AminoAcid
case object GlutamicAcid extends AminoAcid
case object Cysteine extends AminoAcid
case object Tryptophan extends AminoAcid
case object Arginine extends AminoAcid
case object Glycine extends AminoAcid

object AminoAcid {
  def fromCodon(codon: Codon): Option[AminoAcid] = codon match {
    case Codon(G, G, G) => Some(Glycine)
    case Codon(G, G, A) => Some(Glycine)
    case Codon(G, G, U) => Some(Glycine)
    case Codon(G, G, C) => Some(Glycine)

    case Codon(G, A, G) => Some(GlutamicAcid)
    case Codon(G, A, A) => Some(GlutamicAcid)
    case Codon(G, A, U) => Some(AsparticAcid)
    case Codon(G, A, C) => Some(AsparticAcid)

    case Codon(G, U, G) => Some(Valine)
    case Codon(G, U, A) => Some(Valine)
    case Codon(G, U, U) => Some(Valine)
    case Codon(G, U, C) => Some(Valine)

    case Codon(G, C, G) => Some(Alanine)
    case Codon(G, C, A) => Some(Alanine)
    case Codon(G, C, U) => Some(Alanine)
    case Codon(G, C, C) => Some(Alanine)

    case Codon(A, G, G) => Some(Arginine)
    case Codon(A, G, A) => Some(Arginine)
    case Codon(A, G, U) => Some(Serine)
    case Codon(A, G, C) => Some(Serine)

    case Codon(A, A, G) => Some(Lysine)
    case Codon(A, A, A) => Some(Lysine)
    case Codon(A, A, U) => Some(Asparagine)
    case Codon(A, A, C) => Some(Asparagine)

    case Codon(A, U, G) => Some(Methionine) // From an amino acid pov, AUG encodes Methionine, _not_ the start codon.
    case Codon(A, U, A) => Some(Isoleucine)
    case Codon(A, U, U) => Some(Isoleucine)
    case Codon(A, U, C) => Some(Isoleucine)

    case Codon(A, C, G) => Some(Threonine)
    case Codon(A, C, A) => Some(Threonine)
    case Codon(A, C, U) => Some(Threonine)
    case Codon(A, C, C) => Some(Threonine)

    case Codon(U, G, G) => Some(Tryptophan)
    // Codon(U, G, A) encodes the Opal stop codon, returns no AminoAcid
    case Codon(U, G, U) => Some(Cysteine)
    case Codon(U, G, C) => Some(Cysteine)

    // Codon(U, A, G) encodes the Amber stop codon, returns no AminoAcid
    // Codon(U, A, A) encodes the Occur stop codon, returns no AminoAcid
    case Codon(U, A, U) => Some(Tyrosine)
    case Codon(U, A, C) => Some(Tyrosine)

    case Codon(U, U, G) => Some(Leucine)
    case Codon(U, U, A) => Some(Leucine)
    case Codon(U, U, U) => Some(Phenylalanine)
    case Codon(U, U, C) => Some(Phenylalanine)

    case Codon(U, C, G) => Some(Serine)
    case Codon(U, C, A) => Some(Serine)
    case Codon(U, C, U) => Some(Serine)
    case Codon(U, C, C) => Some(Serine)

    case Codon(C, G, G) => Some(Arginine)
    case Codon(C, G, A) => Some(Arginine)
    case Codon(C, G, U) => Some(Arginine)
    case Codon(C, G, C) => Some(Arginine)

    case Codon(C, A, G) => Some(Glutamine)
    case Codon(C, A, A) => Some(Glutamine)
    case Codon(C, A, U) => Some(Histidine)
    case Codon(C, A, C) => Some(Histidine)

    case Codon(C, U, G) => Some(Leucine)
    case Codon(C, U, A) => Some(Leucine)
    case Codon(C, U, U) => Some(Leucine)
    case Codon(C, U, C) => Some(Leucine)

    case Codon(C, C, G) => Some(Proline)
    case Codon(C, C, A) => Some(Proline)
    case Codon(C, C, U) => Some(Proline)
    case Codon(C, C, C) => Some(Proline)

    case _ => None
  }
}

object AminoAcidValidation extends App {
  val rna = RNA(C, A, A, G, G, G, C, U, U, U, C, C, C)
  val xs  = Seq(C, A, A, G, G, G, C, U, U, U, C, C, C)

  def apply() {
    // requires co-variant sequence argument
    require(Some(Glutamine) == AminoAcid.fromCodon(Codon.fromSeq(xs)))
    require(Some(Glutamine) == AminoAcid.fromCodon(Codon.fromSeq(rna)))

    // requires to be orthogonal to cfl., i.e the codons corner-cases:
    require(Some(Methionine) == AminoAcid.fromCodon(Codon.Start))
    require(None == AminoAcid.fromCodon(Codon.Stop.Amber))
    require(None == AminoAcid.fromCodon(Codon.Stop.Occur))
    require(None == AminoAcid.fromCodon(Codon.Stop.Opal))
  }

  apply()
}

