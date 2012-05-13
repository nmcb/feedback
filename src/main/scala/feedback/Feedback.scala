package feedback

/**
 * @author Marco Borst
 * @since 13/05/12
 */

trait Feedback
{
  def ifNotEquals(expected: ScalaObject, actual: ScalaObject)
}
