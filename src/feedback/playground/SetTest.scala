package feedback.playground

/**
 *  *
 */
class SetTest
{
   val strings = Set("a","b")
   val integers = Set(1,2,3)
   val ordered = strings ++ integers
   val contains = ordered.contains(0L) // Should not compile since ordered does not contain Long types.
}
