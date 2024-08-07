//> using options -source:3.6
import scala.language.implicitConversions

trait QueryKey[A]
object QueryKey extends QueryKeyInstances
sealed trait QueryKeyInstances:
  given stringQueryKey: QueryKey[String] = ???

trait QueryValue[-A]
object QueryValue extends QueryValueInstances
sealed trait QueryValueInstances1:
  given stringQueryValue: QueryValue[String] = ???
  given noneQueryValue: QueryValue[None.type] = ???
    // The noneQueryValue makes no sense at this priority. Since QueryValue
    // is contravariant, QueryValue[None.type] is always better than QueryValue[Option[A]]
    // no matter whether it's old or new resolution. So taking both owner and type
    // score into account, it's always a draw. With the new disambiguation, we prefer
    // the optionQueryValue[A], which gives an ambiguity down the road, because we don't
    // know what the  wrapped type A is. Previously, we preferred QueryValue[None.type]
    // because it is unconditional. The solution is to put QueryValue[None.type] in the
    // same trait as QueryValue[Option[A]], as is shown in pos/scala-uri.scala.

sealed trait QueryValueInstances extends QueryValueInstances1:
  given optionQueryValue[A: QueryValue]: QueryValue[Option[A]] = ???

trait QueryKeyValue[A]
object QueryKeyValue:
  given tuple2QueryKeyValue[K: QueryKey, V: QueryValue]: QueryKeyValue[(K, V)] = ???


@main def Test = summon[QueryKeyValue[(String, None.type)]]  // error
