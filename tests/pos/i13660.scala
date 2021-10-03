type Const[A] = [_] =>> A
type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]

type #~>#:[T, R] = FunctionK[Const[T], Const[R]]

object FunctionK:
  def liftConst[A, B](f: A => B): /*FunctionK[Const[A], Const[B]]*/ A #~>#: B =
    [Z1] => (a: A) => f(a)

trait FoldableK[F[_[_], _]]:

  def foldMapK1[A[_], C, B](fa: F[A, C])(f: FunctionK[A, Const[B]]): B

  def toListK[A, C](fa: F[Const[A], C]): List[A] =
    foldMapK1(fa)(FunctionK.liftConst(List(_: A)))
