
import scala.quoted.*

class MVmult[Idx, T, Unt](tring: Ring[T], vec: VecROp[Idx, T, Unt]) {
  private[this] val blas2 = new Blas2(tring, vec)
  import blas2.*
  def mvmult(vout: OVec[Idx, T, Unt], a: Vec[Idx, Vec[Idx, T]], v: Vec[Idx, T]): Unt = vout := a * v
  override def toString(): String = s"MVmult($tring, $vec)"
}

object MVmult {
  def mvmult_p(vout: Array[Int], a: Array[Array[Int]], v: Array[Int]): Unit = {
    val n = vout.length
    val m = v.length

    val vout_ = OVec(n, (i, x: Int) => vout(i) = x)
    val a_ = Vec (n, i => Vec(m, j => a(i)(j)))
    val v_ = Vec (n, i => v(i))

    val MV = new MVmult[Int, Int, Unit](RingInt, new StaticVecR(RingInt))
    MV.mvmult(vout_, a_, v_)
  }

  def mvmult_c(using Quotes): Expr[(Array[Int], Array[Array[Int]], Array[Int]) => Unit] = '{
    (vout, a, v) => {
      val n = vout.length
      val m = v.length
      ${
        val vout_ = OVec('n, (i, x: Expr[Int]) => '{vout($i) = $x})
        val a_ = Vec('n, (i: Expr[Int]) => Vec('m, (j: Expr[Int]) => '{ a($i)($j) } ))
        val v_ = Vec('m, (i: Expr[Int]) => '{v($i)})

        val MV = new MVmult[Expr[Int], Expr[Int], Expr[Unit]](new RingIntExpr, new VecRDyn)
        MV.mvmult(vout_, a_, v_)
      }
    }
  }

  def mvmult_mc(n: Int, m: Int)(using Quotes): Expr[(Array[Int], Array[Array[Int]], Array[Int]) => Unit] = {
    val MV = new MVmult[Int, Expr[Int], Expr[Unit]](new RingIntExpr, new VecRStaDim(new RingIntExpr))
    '{
      (vout, a, v) => {
        if (${Expr(n)} != vout.length) throw new IndexOutOfBoundsException(${Expr(n.toString)})
        if (${Expr(m)} != v.length) throw new IndexOutOfBoundsException(${Expr(m.toString)})
        ${
          val vout_ = OVec(n, (i, x: Expr[Int]) => '{vout(${Expr(i)}) = $x})
          val a_ = Vec(n, i => Vec(m, j => '{ a(${Expr(i)})(${Expr(j)}) } ))
          val v_ = Vec(m, i => '{v(${Expr(i)})})

          MV.mvmult(vout_, a_, v_)
        }
      }
    }
  }

  def mvmult_ac(a: Array[Array[Int]])(using Quotes): Expr[(Array[Int], Array[Int]) => Unit] = {
    import Lifters.*
    '{
      val arr = ${Expr(a)}
      ${
        val (n, m, a2) = amat1(a, 'arr)
        mvmult_abs0(new RingIntPExpr, new VecRStaDyn(new RingIntPExpr))(n, m, a2)
      }
    }
  }

  def mvmult_opt(a: Array[Array[Int]])(using Quotes): Expr[(Array[Int], Array[Int]) => Unit] = {
    import Lifters.*
    '{
      val arr = ${Expr(a)}
      ${
        val (n, m, a2) = amat1(a, 'arr)
        mvmult_abs0(new RingIntOPExpr, new VecRStaDyn(new RingIntPExpr))(n, m, a2)
      }
    }
  }

  def mvmult_roll(a: Array[Array[Int]])(using Quotes): Expr[(Array[Int], Array[Int]) => Unit] = {
    import Lifters.*
    '{
      val arr = ${Expr(a)}
      ${
        val (n, m, a2) = amat1(a, 'arr)
        mvmult_abs0(new RingIntOPExpr, new VecRStaOptDynInt(new RingIntPExpr))(n, m, a2)
      }
    }
  }

  def mvmult_let1(a: Array[Array[Int]])(using Quotes): Expr[(Array[Int], Array[Int]) => Unit] = {
    val (n, m, a2) = amatCopy(a, copy_row1)
    mvmult_abs0(new RingIntOPExpr, new VecRStaOptDynInt(new RingIntPExpr))(n, m, a2)
  }

  def mvmult_let(a: Array[Array[Int]])(using Quotes): Expr[(Array[Int], Array[Int]) => Unit] = {
    initRows(a) { rows =>
      val (n, m, a2) = amat2(a, rows)
      mvmult_abs0(new RingIntOPExpr, new VecRStaOptDynInt(new RingIntPExpr))(n, m, a2)
    }
  }

  def initRows[T: Type](a: Array[Array[Int]])(cont: Array[Expr[Array[Int]]] => Expr[T])(using Quotes): Expr[T] = {
    import Lifters.*
    def loop(i: Int, acc: List[Expr[Array[Int]]]): Expr[T] = {
      if (i >= a.length) cont(acc.toArray.reverse)
      else if (a(i).count(_ != 0) < VecRStaOptDynInt.threshold) {
        val default: Expr[Array[Int]] = '{null.asInstanceOf[Array[Int]]} // never accessed
        loop(i + 1, default :: acc)
      } else '{
        val row = ${Expr(a(i))}
        ${ loop(i + 1, 'row :: acc) }
      }
    }
    loop(0, Nil)
  }

  def amat1(a: Array[Array[Int]], aa: Expr[Array[Array[Int]]])(using Quotes): (Int, Int, Vec[PV[Int], Vec[PV[Int], PV[Int]]]) = {
    val n = a.length
    val m = a(0).length
    val vec: Vec[PV[Int], Vec[PV[Int], PV[Int]]] = Vec(Sta(n), i => Vec(Sta(m), j => (i, j) match {
      case (Sta(i), Sta(j)) => Sta(a(i)(j))
      case (Sta(i), Dyn(j)) => Dyn('{$aa(${Expr(i)})($j)})
      case (i, j) => Dyn('{ $aa(${Dyns.dyni(i)})(${Dyns.dyni(j)}) })
    }))
    (n, m, vec)
  }

  def amat2(a: Array[Array[Int]], refs: Array[Expr[Array[Int]]])(using Quotes): (Int, Int, Vec[PV[Int], Vec[PV[Int], PV[Int]]]) = {
    val n = a.length
    val m = a(0).length
    val vec: Vec[PV[Int], Vec[PV[Int], PV[Int]]] = Vec(Sta(n), i => Vec(Sta(m), j => (i, j) match {
      case (Sta(i), Sta(j)) => Sta(a(i)(j))
      case (Sta(i), Dyn(j)) => Dyn('{(${refs(i)})($j)})
    }))
    (n, m, vec)
  }

  def amatCopy(a: Array[Array[Int]], copyRow: Array[Int] => (Expr[Int] => Expr[Int]))(using Quotes): (Int, Int, Vec[PV[Int], Vec[PV[Int], PV[Int]]]) = {
    val n = a.length
    val m = a(0).length
    val vec: Vec[PV[Int], Vec[PV[Int], PV[Int]]] = Vec(Sta(n), i => Vec(Sta(m), j => (i, j) match {
      case (Sta(i), Sta(j)) => Sta(a(i)(j))
      case (Sta(i), Dyn(j)) =>
        val defrec = copyRow(a(i))
        Dyn(defrec(j))
      case (i, j) => ???
    }))
    (n, m, vec)
  }

  def copy_row1(using Quotes): Array[Int] => (Expr[Int] => Expr[Int]) = v => {
    import Lifters.*
    val arr = Expr(v)
    i => '{ ($arr).apply($i) }
  }

  def copy_row_let(using Quotes): Array[Int] => (Expr[Int] => Expr[Int]) = v => {
    import Lifters.*
    val arr: Expr[Array[Int]] = ??? // FIXME used genlet v
    i => '{ ($arr).apply($i) }
  }

  private def mvmult_abs0(ring: Ring[PV[Int]], vecOp: VecROp[PV[Int], PV[Int], Expr[Unit]])(n: Int, m: Int, a: Vec[PV[Int], Vec[PV[Int], PV[Int]]])(using Quotes): Expr[(Array[Int], Array[Int]) => Unit] = {
    '{
      (vout, v) => {
        if (${Expr(n)} != vout.length) throw new IndexOutOfBoundsException(${Expr(n.toString)})
        if (${Expr(m)} != v.length) throw new IndexOutOfBoundsException(${Expr(m.toString)})
        ${
          val vout_ : OVec[PV[Int], PV[Int], Expr[Unit]] = OVec(Sta(n), (i, x) => '{vout(${Dyns.dyni(i)}) = ${Dyns.dyn(x)}})
          val v_ : Vec[PV[Int], PV[Int]] = Vec(Sta(m), i => Dyn('{v(${Dyns.dyni(i)})}))
          val MV = new MVmult[PV[Int], PV[Int], Expr[Unit]](ring, vecOp)
          MV.mvmult(vout_, a, v_)
        }
      }
    }
  }

}
