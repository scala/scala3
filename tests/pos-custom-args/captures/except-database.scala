import language.experimental.captureChecking
import caps.{Classifier, ExclusiveCapability}

// "Classifying Capabilities" §6.1: a Database classifier tree splitting into two disjoint
// branches, so only[UserTable] flows into except[TransactionTable].
//            Database
//            /       \
//      UserTable   TransactionTable
trait Database         extends Classifier, ExclusiveCapability
trait UserTable        extends Classifier, Database
trait TransactionTable extends Classifier, Database

trait User

class DBImpl extends Database:
  val readUsers: () ->{this.only[UserTable]} List[User] = ???
  val doesntTouchTransactions: () ->{this.except[TransactionTable]} List[User] =
    this.readUsers   // UserTable disjoint from TransactionTable
