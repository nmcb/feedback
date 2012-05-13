package feedback

import graph.Node

/**
 * @author Marco Borst
 * @since 13/05/12
 */

object System extends App
{
    val stdout = Console.out
    val stdin = Console.in

    def test ()
    {
        val node = new Node()
        Feedback.ifNotEquals(node, node)

        val otherNode = new Node()
        Feedback.ifEquals(node, otherNode)

        val referenceToNode = node
        Feedback.ifNotEquals(node, referenceToNode)

        val nodeWithTheSameUUID = new Node("node", null, null, node.uuid)
        Feedback.ifNotEquals(node, nodeWithTheSameUUID)
    }

    test()
}
