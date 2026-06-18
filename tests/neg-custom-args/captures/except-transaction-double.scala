import language.experimental.captureChecking
import language.experimental.erasedDefinitions
import caps.{Classifier, SharedCapability}

// "Classifying Capabilities" §6.2: withTransaction forbids the body's result from capturing
// the transaction's commit/rollback authority. The paper uses two exclusions; the single-
// exclusion form is in pos-custom-args/captures/except-transaction.scala.
trait Commit   extends SharedCapability, Classifier
trait Rollback extends SharedCapability, Classifier

trait Transaction extends SharedCapability:
  erased val cm: Commit^
  erased val rb: Rollback^
  val commit:   () ->{this.only[Commit]} Unit
  val rollback: () ->{this.only[Rollback]} Unit

// TODO: permit multiple except clauses on the same capture reference.
def withTransaction[T, E^](body: (tx: Transaction^) -> (() ->{E, tx.except[Commit].except[Rollback]} T)): T = ??? // error: at most one except clause per capture reference
