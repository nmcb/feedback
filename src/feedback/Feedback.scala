package feedback

import graph.Node

/**
 * @author Marco Borst
 * @since 13/05/12
 */

case class Feedback() extends RuntimeException

object Feedback
{
    def ifEquals(expected: AnyRef, actual: AnyRef)
    {
        if (expected ne actual) return
        if (!expected.equals(actual)) return
        throw new Feedback
    }

    def ifNotEquals(expected: AnyRef, actual: AnyRef)
    {
        if (expected eq actual) return
        if (expected.equals(actual)) return
        throw new Feedback
    }
}
