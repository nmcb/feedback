package feedback.client

import feedback.graph.PlaceGraph
import org.junit.Assert

// Constrained om site and root parent mapping, Any := N -or- Int
case class PM[N](sites: Map[Int, Any], nodes: Map[N, N], roots: Map[Any, Int])

object GraphTest extends App {
   def testWithNodes() {
      val p = PlaceGraph(Map(0 -> 0, 1 -> "a"))(Map("a" -> "b"))(Map("b" -> 1))
      println(p.toString())
   }

   testWithNodes()
}
