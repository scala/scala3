trait MapImpl {
  type Key
  type Value
  type Map
  val lookup: Map => Key => Value
}
import scala.collection.immutable.HashMap

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
    (server: (mImpl: MapImpl { type Key = String; type Value = Int}) => mImpl.Map => String => Int) => Int =
    server => server(HashMapImpl[String, Int])(HashMap())("test lookup key")
  val Client1:
    (server: (mImpl: MapImpl { type Key = String; type Value = Int}) => mImpl.Map => String => Int) => Int =
    server => server(??? : (HashMapImpl[String, Int]))(???)("test lookup key")
  val Client2:
  (server: (mImpl: MapImpl { type Key = String; type Value = Int}) => mImpl.Map => String => Int) => Int =
    server => server(???)(???)("test lookup key")
  val Server1: (mImpl: MapImpl {type Key = String; type Value = Int}) => mImpl.Map => String => Int =
    mImpl => Server0(mImpl)
  implicitly[(mImpl: MapImpl {type Key = String; type Value = Int}) => mImpl.Map => String => Int <:< (mImpl: MapImpl) => mImpl.Map => mImpl.Key => mImpl.Value] // error
  implicitly[(mImpl: MapImpl {type Key = String; type Value = Int}) => mImpl.Map => mImpl.Key => mImpl.Value <:< (mImpl: MapImpl) => mImpl.Map => mImpl.Key => mImpl.Value] // error

  val Result1: Int = Client(Server0) // error
  val Result2: Int = Client(Server1)
  val Result3: Int = Client(mImpl => Server0(mImpl))
}
