sealed trait WIO[+Err, +Out] {

  def handleError[Err1,Out1 >: Out, ErrIn >: Err](
      f: ErrIn => WIO[Err1, Out1]
  ): WIO[Err1, Out1] = HandleError(f)

}

case class HandleError[+ErrOut, +Out, ErrIn](handleError: ErrIn => WIO[ErrOut, Out]) extends WIO[ErrOut, Out]
