package a {

  trait Expr[+T]
  trait QCtx

  /*Quote*/ def q[T](x: T)(using QCtx): Expr[T] = ???
  /*Splice*/ def s[T](x: QCtx ?=> Expr[T]): T = ???
  /*run*/ def r[T](x: QCtx ?=> Expr[T]): T = ???


  val test: Any = {

    def pow(x: Expr[Double], n: Int)(using QCtx): Expr[Double] =
      if n == 0 then q{1.0} else q{ s{x} * s{pow(x, n - 1)} }

    r {
      q{ (x: Double) => s{pow(q{x}, 5)} }
    }

    r {
      q{ (x: Double) =>
        s{
          val y = q{x}
          pow(q{s{y}}, 5)
        }
      }
    }

    r {
      var escaped: Expr[Any] = null
      q{ (x: Double) =>
        s{
          escaped = q{x} // 💥
          pow(q{x}, 5)
        }
      }
    }
  }

}


package b {

  trait QCtx {
    type Expr[+T]
  }

  /*Quote*/ def q[T](x: T)(using qctx: QCtx): qctx.Expr[T] = ???
  /*Splice*/ def s[T](using qctx0: QCtx)(x: (qctx: QCtx { type Expr[+T] >: qctx0.Expr[T] }) ?=> qctx.Expr[T]): T = ???
  /*run*/ def r[T](x: (qctx: QCtx) ?=> qctx.Expr[T]): T = ???


  val test: Any = {

    def pow(using qctx: QCtx)(x: qctx.Expr[Double], n: Int): qctx.Expr[Double] =
      if n == 0 then q{1.0} else q{ s{x} * s{pow(x, n - 1)} }

    r {
      q{ (x: Double) => s{pow(q{x}, 5)} }
    }

    r {
      q{ (x: Double) =>
        s{
          val y = q{x}
          pow(q{s{y}}, 5)
        }
      }
    }

    r { qctx ?=>
      var escaped: qctx.Expr[Double] = ???
      q{ (x: Double) =>
        s{
          escaped = q{x} // error
          pow(q{x}, 5)
        }
      }
    }
  }

}

package c {

  trait QCtx { qctx =>
    type Expr[+T]
    type Type[T]
    type NestedSCtx = SCtx {
      type NestedQCtx = QCtx { type Expr[+T] >: qctx.Expr[T]; type Type[T] >: qctx.Type[T] }
    }
  }

  trait SCtx {
    type NestedQCtx <: QCtx
  }

  /*Quote*/ def q[T](using qctx: QCtx)(x: qctx.NestedSCtx ?=> T): qctx.Expr[T] = ???
  /*Splice*/ def s[T](using sctx: SCtx)(x: (qctx: sctx.NestedQCtx) ?=> qctx.Expr[T]): T = ???
  /*run*/ def r[T](x: (qctx: QCtx) ?=> qctx.Expr[T]): T = ???

  val test: Any = {

    def pow(using qctx: QCtx)(x: qctx.Expr[Double], n: Int): qctx.Expr[Double] =
      if n == 0 then q{1.0} else q{ s{x} * s{pow(x, n - 1)} }

    r {
      q{ (x: Double) => s{pow(q{x}, 5)} }
    }

    r {
      q{ (x: Double) =>
        s{
          val y = q{x}
          pow(q{s{y}}, 5)
        }
      }
    }

    r {
      val a = q{ 4.0 }
      q{ (x: Double) =>
        s{
          pow(q{s{a}}, 5)
        }
      }
    }

    r { qctx ?=>
      var escaped: qctx.Expr[Double] = ???
      q{ (x: Double) =>
        s{
          escaped = q{x} // error
          pow(q{x}, 5)
        }
      }
    }
  }

}

package d {

  import scala.compiletime.S

  trait QCtx { qctx =>
    type Expr[+T]
    // type Type[T]
    type Level <: Int
    type NestedSCtx = SCtx {
      type Level = S[qctx.Level]
      type NestedQCtx = QCtx { type Expr[+T] >: qctx.Expr[T]; type Level = qctx.Level }
    }
  }

  trait SCtx {
    type NestedQCtx <: QCtx
  }

  opaque type Level[N <: Int] = Unit
  object Level {
    /*erased*/ given level[L] as Level[L] = ()
    // given level[L](using QCtx { type }) as Level[L] = 0
  }

  /*Quote*/ def q[T, L <: Int](using qctx: QCtx { type Level = L }, lvl: Level[L])(x: (qctx.NestedSCtx, Level[S[L]]) ?=> T): qctx.Expr[T] = ???
  /*Splice*/ def s[T, L <: Int](using sctx: SCtx { type Level = S[L] }, lvl: Level[S[L]])(x: (qctx: sctx.NestedQCtx, lvl: Level[L]) ?=> qctx.Expr[T]): T = ???
  /*run*/ def r[T, L <: Int](using Level[L])(x: (qctx: QCtx { type Level = L }) ?=> qctx.Expr[T]): T = ???


  val test: Any = {

    def pow(using qctx: QCtx)(x: qctx.Expr[Double], n: Int): qctx.Expr[Double] =
      if n == 0 then q{1.0} else q{ s{x} * s{pow(x, n - 1)} }

    r {
      q{ (x: Double) => s{pow(q{x}, 5)} }
    }

    r {
      q{ (x: Double) =>
        s{
          val y = q{x}
          pow(q{s{y}}, 5)
        }
      }
    }

    r {
      val a = q{ 4.0 }
      q{ (x: Double) =>
        s{
          pow(q{s{a}}, 5)
        }
      }
    }


    r {
      q{
        r {
          q{ 1 }
        }
      }
    }

    r {
      q{
        q{ // error
          1
        }
      }
    }

    r { qctx ?=>
      var escaped: qctx.Expr[Double] = ???
      q{ (x: Double) =>
        s{
          escaped = q{x} // error
          pow(q{x}, 5)
        }
      }
    }
  }

}