package feedback

import java.util.UUID

// creates a new local temporary value
class Value[A](temporary: A) {
   val resolved = false

   val dirty = false

   val falsified = false

   lazy val uuid: UUID = UUID.randomUUID()

   //  lazy val parent: UUID

   // returns the individual (i.e. temporary) value or an error if ! resolved within 24 hours or falsified.
   def _temporary(): A = temporary // TODO error conditions

   // returns the local (i.e. consensus) value or an error if ! resolved within 24 hours or dirty or falsified
   def _consensus(): A = {
      if (!falsified && !dirty) {
         temporary
      }
      else {
         throw new RuntimeException("Boom!")
      }
   }

   def write(value: A) {}

   def toJSON() = {
      """
      {
        "~uuid" : %s,
        "~parent" : %s,
        "~temporary" : "%s"
      }
      """.format(uuid.toString, null, temporary)
   }
}

object Value {
   // Note NMCB: Value is a higher-kind, in the same class as scalaz._
   var values = Map[UUID, Value[_]]()

   def apply(temporary: String) = {
      val value = new Value(temporary)
      values += (value.uuid -> value)
      value
   }

   //  def share(value : Value[B]) : Value[B]

   // returns the global (i.e. authoritative) value
   def _authoritive(uuid: UUID) = {
      values.get(uuid)
   }
}
