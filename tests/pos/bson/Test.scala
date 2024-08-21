//> using options -source 3.5
import bson.*

def stringMapHandler[V](using writer: BSONWriter[Map[String, V]]): BSONHandler[Map[String, V]] = ???
def typedMapHandler[K, V: BSONHandler] = stringMapHandler[V] // warn
