enum Recovery:
  case RecoveryCompleted

enum TypedRecovery:
  case TypedRecoveryCompleted

import Recovery.*
import TypedRecovery.*

class Test {
  TypedRecoveryCompleted match {
    case RecoveryCompleted => println("Recovery completed")            // error
    case TypedRecoveryCompleted => println("Typed recovery completed")
  }

  def foo(x: TypedRecovery) = x match
    case RecoveryCompleted =>             // error
    case TypedRecoveryCompleted =>
}
