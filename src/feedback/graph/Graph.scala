package feedback.graph

import collection._
import scala.Predef._
import collection.Map


trait Graph
{
    def compose (that : Graph) : Graph

    def position (that : Graph) : Graph
}

class PlaceGraph[P] private (val nodes : Map[P, P])
    extends mutable.Map[P, P]
    with mutable.MapLike[P, P, PlaceGraph[P]]
    with Graph
{

    /**
     * Contains this parent graph's path as a radix tree, optimized for child to parent traversal, from sites to roots.
     */
    var suffices : immutable.Map[P, PlaceGraph[P]] = Map.empty

    /**
     * Whether this PlaceGraph's node is present (or just searched for)
     */
    var present : Boolean = false;

    /**
     * Contains the root index, defaults to 0.
     */
    var root : Int = 0

    /**
     * Contains the number of (known) roots, i.e. the root's arity, defaults to 1.
     */
    var roots : Int = 1

    /**
     * Contains an optional site index , defaults to <code>None</code>.
     */
    var site : Option[Traversable[Int]] = None

    /**
     * Contains the number of (known) sites, i.e. the sites arity, defaults to 0.
     */
    var sites : Int = 0

    /**
     * TODO Can we optimize for traversal of (known) child path of this node's PlaceGraph[], i.e. do we want bi-directionality?
     * We could contain references to all (known) child path (searched for or owned) by this PlaceGraph's parent node.
     */
    val children : Traversable[Option[P]] = None

    // path.last == the node we want to return, path.head the root node of the parent node path
    // TODO Is it possible to make this method tail-recursive?  Do we need to?
    def get (path : Seq[P]) : Option[P] =
    {
        if ( path.tail.isEmpty && present )
        {
            Some (path.head)
        }
        else
        {
            suffices get path.head flatMap ( _.get (path.tail) )
        }
    }

    // TODO Is it possible to make this method tail-recursive?  Do we need to?
    def withPath (path : Seq[P]) : PlaceGraph[P] =
    {
        if ( path.isEmpty )
        {
            this
        }
        else
        {
            suffices get path.head match
            {
                case None => suffices = suffices + ( path.head -> empty )
                case _ =>
            }
            suffices (path.head) withPath ( path.tail )
        }
    }

    def knownRoots ()
    {
        // TODO Introduce side-effect? Shall we first ping for new roots?
        roots
    }

    // Traversable

    /**
     * Overwritten for efficiency.
     * TODO Solve a problem that has to do with the implementation of the iterator concatenation method,
     * ++ traversing the sides al previous traversed node paths on one 'side' of the tree (existing or not).
     * We need to traverse existing path only (so root-to-site).  depth or width first?
     */
    override def foreach[U] (f : ( (P, P) ) => U)
    {
        super.foreach (f)
    }


    // Map

    override def update (key : P, value : P)
    {
        super.update (key, value)
    }

    // TODO This doesn't work anymore when we are going to proxy path.
    override def empty = new PlaceGraph[P](Map ())


    // MapLike

    def get (key : P) = null

    def iterator = null

    // Mutable MapLike

    def += (kv : (P, P)) : this.type =
    {

        def path (node : P) : List[P] = nodes get node match
        {
            case None => Nil
            case Some (parent) => node :: path (parent)
        }

        // val path = path(node)
        // withNodePath(path(kv._1)).
        this
    }


    def -= (key : P) : this.type =
    {
        remove (key);
        this
    }

    // Graph

    def compose (that : Graph) = null

    def position (that : Graph) = null
}

object PlaceGraph
{
    def empty[P] = new PlaceGraph[P](Map ())

    def apply[P] (sites : Map[Int, Any])(nodes : Map[P, P])(roots : Map[Any, Int]) : PlaceGraph[P] =
    {


        val places : PlaceGraph[P] = empty
        //    for (site <- sites) m += site
        for ( node <- nodes ) places += node
        //    for (root <- roots) m += root
        places
    }

    type Path[P] = Seq[P]
}