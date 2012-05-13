package feedback

import graph.Node

/**
 * @author Marco Borst
 * @since 13/05/12
 */

object System
{
    def main(args : Array[String])
    {
        val node = new Node("node", null, null)
        Feedback.ifNotEquals(node, node)

        val otherNode = new Node("node", null, null)
        Feedback.ifEquals(node, otherNode)

        val nodeWithTheSameUUID = new Node("node", null, null, node.uuid)
        Feedback.ifNotEquals(node, nodeWithTheSameUUID)

        val referenceToNode = node
        Feedback.ifNotEquals(node, referenceToNode)
    }
}
