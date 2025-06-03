trait Migration
object Migration:
  implicit class MigrationConcat[M <: Migration](m: M):
    def &[N <: Migration, O](n: N)(implicit ccm: CanConcatMigrations[M, N, O]): O = ???

trait ReversibleMigration extends Migration
trait MigrationSeq extends Migration
trait ReversibleMigrationSeq extends MigrationSeq with ReversibleMigration

trait ToReversible[-A <: Migration]
object ToReversible:
  implicit val reversible: ToReversible[ReversibleMigration] = ???
class CanConcatMigrations[-A, -B, +C]
trait CanConcatMigrationsLow:
  implicit def default[A <: Migration, B <: Migration]: CanConcatMigrations[A, B, MigrationSeq] = ???
object CanConcatMigrations extends CanConcatMigrationsLow:
  implicit def reversible[A <: Migration, B <: Migration](implicit reverseA: ToReversible[A],
                                                          reverseB: ToReversible[B]): CanConcatMigrations[A, B, ReversibleMigrationSeq] = ???

@main def Test =
  val rm: ReversibleMigration = ???
  val rms = rm & rm & rm
  summon[rms.type <:< ReversibleMigrationSeq] // error Cannot prove that (rms : slick.migration.api.MigrationSeq) <:< slick.migration.api.ReversibleMigrationSeq.