package feedback.graph

import java.util.UUID

/**
 * @author Marco Borst
 * @since 13/05/12
 */

class Node(val name : String = null, val parent : Node = null, val children : List[Node] = List[Node](), val uuid : UUID = UUID.randomUUID())
{
    @Override
    override def equals(that : Any) : Boolean =
    {

        if (that.isInstanceOf[Node]) {
            this.uuid.equals(that.asInstanceOf[Node].uuid)
        }
        else {
            false
        }
    }
}
