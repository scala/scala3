class Map[K, V]
object Foo {
 val s: Map {type Map$K = String; type Map$V = Int} = null // error
}
