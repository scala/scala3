// This works for implicit/implicit pairs but not for givens, see neg version.
import scala.language.implicitConversions

trait QueryKey[A]
object QueryKey extends QueryKeyInstances
sealed trait QueryKeyInstances:
  implicit val stringQueryKey: QueryKey[String] = ???

trait QueryValue[-A]
object QueryValue extends QueryValueInstances
sealed trait QueryValueInstances1:
  implicit final val stringQueryValue: QueryValue[String] = ???
  implicit final val noneQueryValue: QueryValue[None.type] = ???

sealed trait QueryValueInstances extends QueryValueInstances1:
  implicit final def optionQueryValue[A: QueryValue]: QueryValue[Option[A]] = ???

trait QueryKeyValue[A]
object QueryKeyValue:
  implicit def tuple2QueryKeyValue[K: QueryKey, V: QueryValue]: QueryKeyValue[(K, V)] = ???

@main def Test = summon[QueryKeyValue[(String, None.type)]]
