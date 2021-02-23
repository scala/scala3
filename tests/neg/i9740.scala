abstract class RecoveryCompleted
object RecoveryCompleted extends RecoveryCompleted

abstract class TypedRecoveryCompleted
object TypedRecoveryCompleted extends TypedRecoveryCompleted

class Test {
  TypedRecoveryCompleted match {
    case RecoveryCompleted => println("Recovery completed")            // error
    case TypedRecoveryCompleted => println("Typed recovery completed")
  }

  def foo(x: TypedRecoveryCompleted) = x match
    case RecoveryCompleted =>             // error
    case TypedRecoveryCompleted =>
}
