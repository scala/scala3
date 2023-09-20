//> using options -experimental

import scala.collection.mutable.AbstractSet
import scala.collection.mutable.BitSet
import scala.language.experimental.clauseInterleaving

class Params{
    type U
    def foo[T](x: T)[U >: x.type <: T](using U)[L <: List[U]](l: L): L = ???
    def aaa(x: U): U = ???
    def bbb[T <: U](x: U)[U]: U = ???

    foo[AbstractSet[Int]](BitSet())[AbstractSet[Int]](using BitSet())[List[AbstractSet[Int]]](List[AbstractSet[Int]]())
}

class Param2 extends Params {
    type U = AbstractSet[Int]

    aaa(BitSet())
    bbb[BitSet](BitSet())[AbstractSet[Int]]
}