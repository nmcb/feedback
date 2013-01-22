package feedback.graph

import java.util.UUID
import collection.BitSet


sealed abstract class Value[A] protected (val uuid : UUID = UUID.randomUUID ())
{
    def value : A

    //  def apply(set : BitSet) : Value =  set
}

case class Node[A] (value : A) extends Value[A]

object Value
{
    //
    //
    //  fromBitSet(set : BitSet)
}
