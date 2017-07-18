trait A { type L }
trait B extends A { type L[X] } // error: illegal override

trait A2 { type L <: String }
trait B2 extends A2 { type L[X] <: String } // error: illegal override
trait C2 extends B2 { type L[X, Y] <: String } // error: illegal override

trait D { type I }
trait E extends D { type I <: String }
trait F extends D { type I >: String }
trait G extends E with F // ok

trait H extends D { type I >: Int }
trait H2 extends E with H // error: illegal override
