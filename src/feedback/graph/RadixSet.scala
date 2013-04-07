package feedback.graph

import collection._
import scala.Some
import java.util.UUID

class PRNT[N](val nodes: Set[N], val parents: Map[N, N], val roots: Map[Any, Int] = Map(), val sites: Map[Int, Any] = Map()) {
   override def toString = "PRNT(" + nodes + ", " + parents.toString() + ", " + roots.toString() + ", " + sites.toString() + ")"
}

object PRNT extends App {
   def apply[N](nodes: Set[N], parents: Map[N, N], roots: Map[Any, Int] = Map(), sites: Map[Int, Any] = Map()): PRNT[N] = {
      new PRNT(nodes, parents, roots, sites)
   }

   def run() {
      abstract class Node
      case class Wissel(id: Int) extends Node
      case class Steller(id: Int) extends Node

      val gp = PRNT(
                      List(Steller(1), Wissel(1)).toSet[Node],
                      Map[Node, Node](Steller(1) -> Wissel(1)),
                      Map(Wissel(1) -> 0),
                      Map(0 -> 0, 1 -> Steller(1))
                   )

      val hp = PRNT(
                      Set[Node](Steller(2), Wissel(2)),
                      Map[Node, Node](Steller(2) -> Wissel(2)),
                      Map(Wissel(2) -> 0),
                      Map(0 -> 0)
                   )

      val ip = PRNT(
                      gp.nodes ++ hp.nodes,
                      gp.parents ++ hp.parents,
                      gp.roots ++ hp.roots,
                      gp.sites ++ gp.sites
                   )

      println("GP=" + gp)
      println("HP=" + hp)
      println("IP=" + ip)
   }

   run()
}

/**
 *
 * @tparam N
 */
class RadixMap[N]

// prefix : Seq[N] := [root-node..some-node]
   extends Set[Seq[N]]

   // prefix : Seq[N] := [root-node..some-node]
   with SetLike[Seq[N], RadixMap[N]] {
   /**
    * radices : N => RadixMap[N] := some-node => that-node's-radix
    **/
   var radices: Map[N, RadixMap[N]] = Map.empty

   /**
    * Contains some node's optional value.
    */
   var value: Option[N] = None

   /**
    * Contains some node's optional root. (i.e. authority index)
    */
   var root: Option[Int] = None

   override def empty = new RadixMap[N]

   def +(path: Seq[N]) = {
      path.length match {
         case 0 => root = Some(0)
         case 1 => withPath(path) value = Some(path.head)
         case _ => withPath(path.init) + (Seq(path.last)) // perf.opt. jump with path init to add method
      }
      this
   }

   def -(path: Seq[N]) = {
      path.length match {
         case 0 => root = None
         case 1 => withPath(path) value = None
         case _ => withPath(path.init) - (Seq(path.last)) // perf.opt. jump with path init to remove method
      }
      this
   }

   def contains(path: Seq[N]): Boolean = {
      withPath(path) value match {
         case Some(node) => true
         case None => false
      }
   }

   def iterator: Iterator[Seq[N]] = {
      (for (v <- value.iterator) yield Nil) ++ (for ((node, radix) <- radices.iterator; path <- radix.iterator) yield (node +: path))
   }

   def withPath(path: Seq[N]): RadixMap[N] = {
      path match {
         case Nil => {
            this
         }
         case _ => {
            radices get path.head match {
               case None => {
                  radices = radices + (path.head -> empty)
               }
               case _ =>
            }
            radices(path.head) withPath (path.tail)
         }
      }
   }
}

object RadixMap extends App {
   def apply[N](paths: Seq[N]*): RadixMap[N] = {
      var map = new RadixMap[N]
      for (path <- paths) map = map + path
      map
   }

   private def path[N](node: N, parents: Map[N, N]): Seq[N] = {
      def recurse(node: N): Seq[N] = parents get node match {
         case Some(parent: N) => node +: recurse(parent)
         case None => Seq(node)
      }
      recurse(node)
   }

   def apply[N](nodes: Set[N], parents: Map[N, N] = Map[N, N]()): RadixMap[N] = {
      var map = new RadixMap[N]
      for (node <- nodes) {
         map = map + path(node, parents)
      }
      map
   }

   def downStream = null

   def upStream = null

   def testApply() {
      var radixFromPaths = RadixMap(Seq("[0]"), Seq("[0]", "a"), Seq("[0]", "b"), Seq("[0]", "b", "d"), Seq("[0]", "b", "c"), Seq("[0]", "a", "l"), Seq("[0]", "a", "l", "l"))
      println("radixFromPaths = " + radixFromPaths)
      radixFromPaths.iterator.toList.foreach(x => println("-> " + x))
      println()

      val nodes = Set("a", "b", "c", "d", "e")
      val parentNodes = Map("d" -> "b", "c" -> "b", "e" -> "a")
      var radixFromNodesAndParentNodes = new RadixMap[String]

      println("nodes = " + nodes)
      println("parent-nodes = " + parentNodes)
      for (node <- nodes) {
         val p = path(node, parentNodes)
         println("-> path(" + node + ") := " + p)
         radixFromNodesAndParentNodes = radixFromNodesAndParentNodes + p
      }
      println()

      radixFromNodesAndParentNodes = RadixMap(nodes, parentNodes)
      println("radix-prnt = " + radixFromNodesAndParentNodes)
      radixFromNodesAndParentNodes.iterator.toList.foreach(x => println("-> " + x))
      println()

      sealed abstract class Value(/* uuid : UUID = UUID.randomUUID (), */ val uuid: Any) {
         def equals(that: Value) = this.uuid.equals(that.uuid)

         override def hashCode() = uuid.hashCode()
      }
      case class Wissel(/* @id */ name: String, voor: Value, links: Value, rechts: Value) extends Value(name)
      case class Connector(index: Int) extends Value(index.toString)
      case class Ref(id: Any) extends Value(id)

      //val radixOfConnectors = RadixMap (Set ("a", "b", "c", "d", "e"))
      val radixOfConnectors = RadixMap(Set(Ref(0), Connector(1), Connector(2), Connector(3), Connector(4), Connector(5)))
      println("radixOfConnectors = " + radixOfConnectors)
      radixOfConnectors.iterator.toList.foreach(x => println("-> " + x))
      println()

      val wa = Wissel("a", Connector(0), Ref("b"), Connector(3))
      val wb = Wissel("b", Ref("a"), Connector(2), Connector(3))
      val wc = Wissel("c", Connector(1), Connector(4), Connector(5))
      val radixOfWissels = RadixMap(
                                      Seq(wa),
                                      Seq(wb, wa),
                                      Seq(wc, wa)
                                   )
      println("radixOfWissels = " + radixOfWissels)
      radixOfWissels.iterator.toList.foreach(x => println("-> " + x))
      println()

      val radixOfConnectorsWithWissels = (radixOfConnectors ++ radixOfWissels)
      println("radixOfConnectorsWithWissels = " + radixOfConnectorsWithWissels)
      radixOfConnectorsWithWissels.iterator.toList.foreach(x => println("-> " + x))
      println()
   }

   testApply()
}

class Dummy {
   //  extends Set[(N,N)] // child -> parent
   //  with SetLike[(N,N), RadixMap[N]] {
   //
   //
   //  def +(elem: (N, N)) = null
   //
   //  def -(elem: (N, N)) = null
   //
   //  def contains(elem: (N, N)) = false
   //
   //  def iterator = null
   //
   // def withChild(n : N) = null

   //  extends Set[N => N] // child -> parent
   //  with SetLike[N => N, RadixMap[N]] {
   //
   //  def +(elem: (N) => N) = null
   //
   //  def -(elem: (N) => N) = null
   //
   //  def contains(elem: (N) => N) = {
   //    radices match {
   //      case (digit, radix) =>
   //    }
   //  }
   //
   //  def iterator = null

   //  extends Map[N, N] // child -> parent
   //  with MapLike[N,N, RadixMap[N]] {
   //
   //  def +[B1 >: N](kv: (N, B1)) = null
   //
   //  def -(key: N) = null
   //
   //  def get(key: N) = null
   //
   //  def iterator = null
   //
   // def withChild(n : N)

}


