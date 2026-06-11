/* 
    This is a special case for inline traits because the summon method is defined inline. We don't evaluate it until after 
    we've inlined it into child traits, but this means it can hang around in the original inline trait, which is a problem because
    it throws a compiler error "method should have been inlined but was not". We need to make sure we prune it out, which initially
    was missing for private fields (since it wasn't clear that this was necessary, but it is).
*/

inline trait A[T: Numeric]:
    private val v = summon[Numeric[T]]

inline trait B[T: Numeric] extends A[T]

class C extends B[Int], A[Int]
