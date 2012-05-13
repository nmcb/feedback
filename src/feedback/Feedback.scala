package feedback

/**
 * @author Marco Borst
 * @since 13/05/12
 */

class Feedback extends RuntimeException

object Feedback
{
  def ifNotEquals(expected: AnyRef, actual: AnyRef)
  {
    if (expected eq actual) return
    if (expected equals actual) return
    throw new Feedback
  }
}
