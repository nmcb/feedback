package feedback.immutable

import scala.collection.SetLike
import scala.collection.generic.{GenericSetTemplate, GenericCompanion, CanBuildFrom}
import scala.collection.mutable.{Builder, SetBuilder}

class Foo
class Bar extends Foo

class FooSet(seq : Foo*) extends Set[Foo]
                             with SetLike[Foo, FooSet]
                             with Serializable {
    override def empty: FooSet = new FooSet()
    def + (elem: Foo) : FooSet = if (seq contains elem) this
        else new FooSet(elem +: seq: _*)
    def - (elem: Foo) : FooSet = if (!(seq contains elem)) this
        else new FooSet(seq filterNot (elem ==): _*)
    def contains (elem: Foo) : Boolean = seq exists (elem ==)
    def iterator : Iterator[Foo] = seq.iterator
}

object FooSet {
    def empty: FooSet = new FooSet()
    def newBuilder: Builder[Foo, FooSet] = new SetBuilder[Foo, FooSet](empty)
    def apply(elems: Foo*): FooSet = (empty /: elems) (_ + _)
    def thingSetCanBuildFrom = new CanBuildFrom[FooSet, Foo, FooSet] {
        def apply(from: FooSet) = newBuilder
        def apply() = newBuilder
    }
}