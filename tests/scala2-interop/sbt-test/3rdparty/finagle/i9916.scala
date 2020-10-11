import com.twitter.finagle.Stack
import com.twitter.finagle.liveness.FailureAccrualFactory

trait T {
  def p: FailureAccrualFactory.Param
}

class A {
  val t = Stack.Param[FailureAccrualFactory.Param](FailureAccrualFactory.Param(???))
}
