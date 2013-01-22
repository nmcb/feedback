package feedback

import org.junit.Assert
import feedback.CC_Graph.{_PlaceOrdering, PlaceOrdering}

/**
 * @author Marco Borst
 * @since 13/05/12
 */

trait Indexed
{
    val index : Int
}

trait Place extends Indexed with PlaceOrdering

case class Node (value : Any) extends Place
{
    val index = value.hashCode
}

case class Root (index : Int) extends Place

case class Site (index : Int) extends Place

case class Edge (value : Any) extends Indexed
{
    val index = value.hashCode
}

case class Port (index : Int) extends Indexed

case class Name (value : String) extends Indexed
{
    val index = value.hashCode
}

case class Inner (index : Int = 0) extends Indexed

case class Outer (index : Int = 1) extends Indexed

case class CC_Open (index : Int = 0) extends Indexed

case class CC_Closed (index : Int = 1) extends Indexed


//class Node(val name : String = null, val parent : Node = null, val children : List[Node] = List[Node](), val uuid : UUID = UUID.randomUUID())
//{
//    @Override
//    override def equals(that : Any) : Boolean =
//    {
//
//        if (that.isInstanceOf[Node]) {
//            this.uuid.equals(that.asInstanceOf[Node].uuid)
//        }
//        else {
//            false
//        }
//    }
//}

//trait CC_PlaceGraph[C <: Place, P <: Place](
//                                          path : Set[Node],
//                                          ctrl  : Map[Node,(String, Int)],
//                                          prnt  : Map[C,P]
//                                          )


case class CC_Graph[C <: Place, P <: Place] (
                                                nodes : Set[Node],
                                                edges : Set[Edge],
                                                prnt : Map[C, P],
                                                ctrl : Map[Node, (String, Int)],
                                                link : Map[
                                                    _ <: (_ <: Indexed, _ <: Indexed),
                                                    _ <: (_ <: Indexed, _ <: Indexed)
                                                    ]
                                                )
{

    def rootOf (place : C) : Root = prnt (place) match
    {
        case root : Root => root
        case child : C => rootOf (child)
    }

    def sites = ( for ( place <- prnt.keys if place.isInstanceOf[Site] ) yield place ).toList.sorted (_PlaceOrdering)

    def roots = ( for ( place <- prnt.values if place.isInstanceOf[Root] ) yield place ).toList.sorted (_PlaceOrdering)

    def controls = ( for ( control <- ctrl.values ) yield control ).toSet

    def placeGraph = (nodes, ctrl, prnt)

    ////  def linkGraph = PlaceGraph(path, edges, ctrl, link)
    ////
    //  val s = new Set().++()
    //  def compose(that : CC_Graph) : CC_Graph = {
    //    CC_Graph(this.placeGraph ++ that.placeGraph, this.linkGraph ++ that.linkGraph)
    //  }

    //  def compose(that: CC_Graph): CC_Graph = {
    //    new CC_Graph(
    //      this.path ++ that.path,
    //      this.edges ++ that.edges,
    //      resolve(this.prnt, that.prnt),
    //      superimpose(this.ctrl, that.ctrl)
    //      resolve(this.link, that.link)
    //    )
    //  }
}


object CC_Graph extends App
{
    val stdout = Console.out
    val stdin = Console.in

    run ()

    def run ()
    {

        val h = CC_Graph (
            Set (Node (0), Node (2)),
            Set (Edge (0)),
            Map (
                Site (0) -> Node (0),
                Site (1) -> Node (2),
                Site (2) -> Root (1),
                Node (0) -> Root (0),
                Node (2) -> Node (0)
            ),
            Map (
                Node (0) -> ("E", 1)
            ),
            Map (
                (Inner (), Name ("x")) -> (CC_Open (), Edge (0)),
                (Inner (), Name ("y")) -> (CC_Open (), Edge (0)),
                (Node (0), Port (0)) -> (CC_Open (), Edge (0))
            )
        )
        println ("~h:" + h)

        println ("~h.sites:" + h.sites)
        Assert.assertEquals (3, h.sites.size)
        Assert.assertEquals (Site (0), h.sites (0))
        Assert.assertEquals (Site (1), h.sites (1))
        Assert.assertEquals (Site (2), h.sites (2))

        println ("~h.roots:" + h.roots)
        Assert.assertEquals (2, h.roots.size)
        Assert.assertEquals (Root (0), h.roots (0))
        Assert.assertEquals (Root (1), h.roots (1))

        Assert.assertEquals (Root (0), h.rootOf (Site (0)))
        Assert.assertEquals (Root (0), h.rootOf (Site (1)))
        Assert.assertEquals (Root (1), h.rootOf (Site (2)))
        Assert.assertEquals (Root (0), h.rootOf (Node (0)))
        Assert.assertEquals (Root (0), h.rootOf (Node (2)))

        println ("~h.controls:" + h.controls)
        Assert.assertEquals (1, h.controls.size)
        Assert.assertTrue (h.controls.contains ("E", 1))

        // COMPOSE

        val f_nodes = Set (Node (1), Node (3), Node (4), Node (5))
        val f_edges = Set (Edge (1), Edge (2))
        val f_prnt = Map (
            Node (1) -> Root (0),
            Node (3) -> Root (1),
            Node (4) -> Root (2),
            Node (5) -> Node (4)
        )
        val f_ctrl = Map (
            Node (1) -> ("C", 2),
            Node (3) -> ("C", 2),
            Node (4) -> ("C", 2),
            Node (5) -> ("D", 1)
        )
        val f_link = Map (
            (Node (0), Port (0)) -> (Outer (), Edge (0)),
            (Node (1), Port (0)) -> (Outer (), Edge (0)),
            (Node (1), Port (1)) -> (Outer (), Edge (1)),
            (Node (3), Port (0)) -> (Outer (), Edge (1)),
            (Node (3), Port (1)) -> (Outer (), Edge (2)),
            (Node (4), Port (0)) -> (Outer (), Edge (2)),
            (Node (4), Port (1)) -> (Outer (), Edge (0)),
            (Node (5), Port (0)) -> (Outer (), Edge (2))
        )
        val f_bigraph = new CC_Graph (f_nodes, f_edges, f_prnt, f_ctrl, f_link)
        println ("~f:" + f_bigraph)

        // EQUALS

        val g_nodes = Set (Node (0), Node (1), Node (2), Node (3), Node (4), Node (5))
        val g_edges = Set (Edge (0), Edge (1), Edge (2))
        val g_prnt = Map (
            Node (0) -> Root (0),
            Node (1) -> Node (0),
            Node (2) -> Node (0),
            Node (3) -> Node (2),
            Node (4) -> Root (1),
            Node (5) -> Node (4)
        )
        val g_ctrl = Map (
            Node (0) -> ("E", 1),
            Node (1) -> ("C", 2),
            Node (3) -> ("C", 2),
            Node (4) -> ("C", 2),
            Node (5) -> ("D", 1)
        )
        val g_link = Map (
            (Node (0), Port (0)) -> (CC_Closed (), Edge (0)),
            (Node (1), Port (0)) -> (CC_Closed (), Edge (0)),
            (Node (1), Port (1)) -> (CC_Closed (), Edge (1)),
            (Node (3), Port (0)) -> (CC_Closed (), Edge (1)),
            (Node (3), Port (1)) -> (CC_Closed (), Edge (2)),
            (Node (4), Port (0)) -> (CC_Closed (), Edge (2)),
            (Node (4), Port (1)) -> (CC_Closed (), Edge (0)),
            (Node (5), Port (0)) -> (CC_Closed (), Edge (2))
        )
        val g_bigraph = new CC_Graph (g_nodes, g_edges, g_prnt, g_ctrl, g_link)
        println ("~g:" + g_bigraph)
    }

    trait PlaceOrdering extends Ordering[Place]
    {
        def compare (a : Place, b : Place) = a.index compare b.index
    }

    implicit object _PlaceOrdering extends PlaceOrdering

}

//class Agent(name: String) extends Value[String](name)
//
//class Room(number: Int) extends Value[Int](number)
//
//object Blah {
//  def test() {
//    val node = Node(1)
//    Feedback.ifNotEquals(Node(1), node)
//
//    val otherNode = new Node(1)
//    Feedback.ifNotEquals(otherNode, node)
//
//    val referenceToNode = node
//    Feedback.ifNotEquals(node, referenceToNode)
//
//  }
//
//  test()
//
//}