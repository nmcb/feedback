package feedback.graph

import collection._

class Byte
{
}

object Byte
{
    // The number of information slots
    private val S = 2
    // The number of information bits that can be represented in a slot
    private val N = 128
    // The number of uniqueness bits needed to represent that information 's UUID
    private val U = 128

    // information indices
    private val N_MSB = 0x00000000
    private val N_LSB = 0x00000000
    private val U_MSB = 0x00000000
    private val U_LSB = 0x00000000

    //  def fromIntSeq(ints : Seq[Int]) : Byte = ints take(4) match {
    //    case (uMSB, uLSB, nMSB, nLSB) => _
    //    case _                        => _
    //  }
}
