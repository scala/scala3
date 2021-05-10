package A {
    opaque type T = Int
    def t: T = 0
}

package B {
    export A.T
    val t: B.T = A.t
}
