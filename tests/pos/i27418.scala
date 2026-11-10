class ET[A]:
  def flatMap[AA >: A](f: Unit => ET[AA]): ET[AA] = ???

def liftF[A]: ET[A] = ???

def first: ET[Int] = ???
def second: ET[String | Boolean] = ???
def third: ET[Double] = ???
def fourth: ET[Long] = ???

def process =
  first.flatMap(_ =>
    second.flatMap(_ =>
      liftF.flatMap(_ =>
        liftF.flatMap(_ =>
          third.flatMap(_ =>
            fourth.flatMap(_ =>
              liftF.flatMap(_ =>
                liftF)))))))

// A variant that was already failing before the regression (back to 3.3.x)
object SameButOnlyLifts:
  def second: ET[Int | String] = ???
  def process = second.flatMap(_ => liftF.flatMap(_ => liftF.flatMap(_ => liftF.flatMap(_ => liftF))))
