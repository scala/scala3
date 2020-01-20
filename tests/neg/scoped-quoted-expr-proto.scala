package a {

  trait Expr[+T]
  trait QCtx

  /*Quote*/ def q[T](x: T)(given QCtx): Expr[T] = ???
  /*Splice*/ def s[T](x: (given QCtx) => Expr[T]): T = ???
  /*run*/ def r[T](x: (given QCtx) => Expr[T]): T = ???


  val test: Any = {

    def pow(x: Expr[Double], n: Int)(given QCtx): Expr[Double] =
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

  /*Quote*/ def q[T](x: T)(given qctx: QCtx): qctx.Expr[T] = ???
  /*Splice*/ def s[T](given qctx0: QCtx)(x: (given qctx: QCtx { type Expr[+T] >: qctx0.Expr[T] }) => qctx.Expr[T]): T = ???
  /*run*/ def r[T](x: (given qctx: QCtx) => qctx.Expr[T]): T = ???


  val test: Any = {

    def pow(given qctx: QCtx)(x: qctx.Expr[Double], n: Int): qctx.Expr[Double] =
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

    r { (given qctx) =>
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
