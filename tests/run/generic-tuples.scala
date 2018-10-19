package tuples {

trait Tuple

/** () in stdlib */
class HNil extends Tuple
case object HNil extends HNil

trait Pair[H, T <: Tuple] {
  erased inline def size = ???
}
}


object Test extends App {

}