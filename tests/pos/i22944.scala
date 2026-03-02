trait Wrapper {
  type Member
}

object Wrapper {

  type Aux[M] =
    Wrapper {
      type Member = M
    }

}

// note: the `private` and `private val` are probably not necessary
final case class Demo[M] private (
    private val service: Wrapper.Aux[M],
    private val endpoints: Vector[service.Member]
)

trait Servo[Alg[_[_]]] {

  type Operation[I]
}

object Servo {

  type Aux[Alg[_[_]], Op[_]] =
    Servo[Alg] {
      type Operation[I] = Op[I]
    }

}

trait MyEndpoint[Op[_]]

final case class BSPBuilder2[Alg[_[_]], Op[_]] private (
    private val service: Servo.Aux[Alg, Op],
    private val endpoints: Vector[MyEndpoint[service.Operation]]
)