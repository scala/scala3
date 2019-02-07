package example

class DepTemp {

}

abstract class DepAdvD[CC[X[C] <: B], X[Z], C] extends DepTemp {
val foo: List[Option[Option[X[C]]]]
}