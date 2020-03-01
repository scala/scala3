import scala.collection.immutable.HashMap

trait MapImpl {
  type Key
  type Value
  type Map
  val lookup: Map => Key => Value
}
class HashMapImpl[K, V] extends MapImpl {
  type Key = K
  type Value = V
  type Map = HashMap[K, V]
  val lookup: Map => Key => Value = m => k => m(k)
}
object Foo {
  val Server0:
    (mImpl: MapImpl) => mImpl.Map => mImpl.Key => mImpl.Value
    = mImpl => mImpl.lookup
  val Client:
    (server: (mImpl: MapImpl & {type Key = String} & {type Value = Int}) => mImpl.Map => String => Int) => Int =
    // server => server(??? : (HashMapImpl[String, Int]))(???)("test lookup key") //works
    // server => server(HashMapImpl[String, Int])(???)("") //works
    server => server(???)(???)("test lookup key") // error
}