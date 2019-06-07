
import scala.quoted._
import scala.quoted.autolift._

trait VecROp[Idx, T, Unt] extends VecOp[Idx, Unt] {
  def reduce: ((T, T) => T, T, Vec[Idx, T]) => T
}

class StaticVecR[T](r: Ring[T]) extends VecSta with VecROp[Int, T, Unit] {
  import r._
  def reduce: ((T, T) => T, T, Vec[Int, T]) => T = { (plus, zero, vec) =>
    var sum = zero
    for (i <- 0 until vec.size)
      sum = plus(sum, vec(i))
    sum
  }
  override def toString(): String = s"StaticVecR($r)"
}

class VecRDyn[T: Type] extends VecDyn with VecROp[Expr[Int], Expr[T], Expr[Unit]] {
  def reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Expr[Int], Expr[T]]) => Expr[T] = {
    (plus, zero, vec) => '{
      var sum = $zero
      var i = 0
      while (i < ${vec.size}) {
        sum = ${ plus('sum, vec('i)) }
        i += 1
      }
      sum
    }
  }
  override def toString(): String = s"VecRDyn"
}

class VecRStaDim[T: Type](r: Ring[T]) extends VecROp[Int, T, Expr[Unit]]  {
  val M = new StaticVecR[T](r)
  def reduce: ((T, T) => T, T, Vec[Int, T]) => T = M.reduce
  val seq: (Expr[Unit], Expr[Unit]) => Expr[Unit] = (e1, e2) => '{ $e1; $e2 }
  // val iter:  (arr: Vec[]) = reduce seq .<()>. arr
  def iter: Vec[Int, Expr[Unit]] => Expr[Unit] = arr => {
    def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
      if (i < arr.size) loop(i + 1, '{ $acc; ${arr.get(i)} })
      else acc
    loop(0, '{})
  }
  override def toString(): String = s"VecRStaDim($r)"
}

class VecRStaDyn[T : Type : Liftable](r: Ring[PV[T]]) extends VecROp[PV[Int], PV[T], Expr[Unit]] {
  val VSta: VecROp[Int, PV[T], Expr[Unit]] = new VecRStaDim(r)
  val VDyn = new VecRDyn
  val dyn = Dyns.dyn[T]
  def reduce: ((PV[T], PV[T]) => PV[T], PV[T], Vec[PV[Int], PV[T]]) => PV[T] = { (plus, zero, vec) => vec match {
      case Vec(Sta(n), v) => VSta.reduce(plus, zero, Vec(n, i => v(Sta(i))))
      case Vec(Dyn(n), v) => Dyn(VDyn.reduce((x, y) => dyn(plus(Dyn(x), Dyn(y))), dyn(zero), Vec(n, i => dyn(v(Dyn(i))))))
    }
  }
  def iter: Vec[PV[Int], Expr[Unit]] => Expr[Unit] =  arr => {
    arr.size match {
      case Sta(n) =>
        def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
          if (i < n) loop(i + 1, '{ $acc; ${arr.get(Sta(i))} })
          else acc
        loop(0, '{})
      case Dyn(n) =>
          '{ "TODO"; () }

    }
  }
  override def toString(): String = s"VecRStaDim($r)"
}

object VecRStaOptDynInt {
  val threshold = 3
}

class VecRStaOptDynInt(r: Ring[PV[Int]]) extends VecRStaDyn(r) {
  val M: VecROp[PV[Int], PV[Int], Expr[Unit]] = new VecRStaDyn(r)

  override def reduce: ((PV[Int], PV[Int]) => PV[Int], PV[Int], Vec[PV[Int], PV[Int]]) => PV[Int] = (plus, zero, vec) => vec match {
    case Vec(Sta(n), vecf) =>
      if (count_non_zeros(n, vecf) < VecRStaOptDynInt.threshold) M.reduce(plus, zero, vec)
      else M.reduce(plus, zero, Vec(Dyn(n), vecf))
    case _ => M.reduce(plus, zero, vec)
  }

  private def count_non_zeros(n: Int, vecf: PV[Int] => PV[Int]): Int = {
    def loop(i: Int, acc: Int): Int = {
      if (i >= n) acc
      else loop(i + 1, if (vecf(Sta(i)) == Sta(0)) acc else acc + 1)
    }
    loop(0, 0)
  }
}
