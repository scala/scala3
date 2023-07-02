
// Test case for dependent types
trait DSL {
    type N
    def toString(n: N): String
    val zero: N
    def next(n: N): N
}

object IntDSL extends DSL {
    type N = Int
    def toString(n: N): String = n.toString()
    val zero = 0
    def next(n: N): N = n + 1
}
