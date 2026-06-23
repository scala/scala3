import language.experimental.captureChecking
import language.experimental.erasedDefinitions
import caps.{Classifier, SharedCapability}

// Section 6.2 of "Classifying Capabilities": the withTransaction pattern restricts a
// specific reference rather than banning a whole effect class. Here we use a single
// exclusion — a view of the transaction that drops its commit capability; the
// two-exclusion form the paper uses is in except-transaction-double.scala.
trait Commit   extends SharedCapability, Classifier
trait Rollback extends SharedCapability, Classifier

trait Transaction extends SharedCapability:
  erased val cm: Commit^
  erased val rb: Rollback^
  val commit:   () ->{this.only[Commit]} Unit
  val rollback: () ->{this.only[Rollback]} Unit

def test(tx: Transaction^): Unit =
  val limited: Object^{tx.except[Commit]} = ???
  val full:    Object^{tx}                = limited   // {tx.except[Commit]} <: {tx}
  val rb:      Object^{tx.only[Rollback]} = ???
  // the rollback part survives the exclusion of commit, since Rollback and Commit
  // are disjoint classifiers
  val viaExc:  Object^{tx.except[Commit]} = rb
