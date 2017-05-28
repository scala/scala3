class Map[K]
object Foo {
 type X = Map { type Map$$K = String } // error
}
