class Error {
  abstract class RecoveryCompleted
  case object RecoveryCompleted extends RecoveryCompleted {
    override def equals(that: Any): Boolean = that == TypedRecoveryCompleted
  }

  abstract class TypedRecoveryCompleted
  case object TypedRecoveryCompleted extends TypedRecoveryCompleted


  TypedRecoveryCompleted match {
    case RecoveryCompleted => println("Recovery completed")
    case TypedRecoveryCompleted => println("Typed recovery completed")
  }
}

object Test {
  def main(args: Array[String]): Unit = new Error
}