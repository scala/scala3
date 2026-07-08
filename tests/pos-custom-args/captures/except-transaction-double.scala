import language.experimental.captureChecking
import language.experimental.erasedDefinitions
import caps.{Classifier, SharedCapability}

// "Classifying Capabilities" §6.2: withTransaction forbids the body's result from capturing
// the transaction's commit/rollback authority, using two exclusions. The single-exclusion
// form is in except-transaction.scala.
trait Commit   extends SharedCapability, Classifier
trait Rollback extends SharedCapability, Classifier

trait Transaction extends SharedCapability:
  erased val cm: Commit^
  erased val rb: Rollback^
  val commit:   () ->{this.only[Commit]} Unit
  val rollback: () ->{this.only[Rollback]} Unit

def withTransaction[T, E^](body: (tx: Transaction^) -> (() ->{E, tx.except[Commit].except[Rollback]} T)): T = ???
