
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

trait Resource[F[_], A]
trait IO[A]

trait Cache[F[_], K, V]

object Cache {

  final case class Config(expireAfterRead: FiniteDuration)

  def expiring[F[_], K, V](
      expireAfter: FiniteDuration
  ): Resource[F, Cache[F, K, V]] = ???

  def expiring[F[_], K, V](
      config: Config,
      partitions: Option[Int] = None
  ): Resource[F, Cache[F, K, V]] = ???

  /* Without partitions being specified, error is yielded */

  val notCompiling = expiring[IO, Int, Int](
    config = Config(expireAfterRead = 1.minute)
  )

  val compiling = expiring[IO, Int, Int](
    config = Config(expireAfterRead = 1.minute),
    partitions = None
  )
}
