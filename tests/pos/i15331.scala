object Test:
  trait Composable[A,B]:
    def compose(a: A, b: B): Any

  trait Arrow {type Dom; type Codom}

  given composeArrows[A, Arr1 <: Arrow, Arr2 <: Arrow]: Composable[Arr1 {type Dom = A}, Arr2 {type Codom = A}] with
    def compose(a: Arr1 {type Dom = A}, b: Arr2 {type Codom = A}): Arrow {type Dom = b.Dom; type Codom = a.Codom} = ???

  object arr1 extends Arrow { type Dom = Int; type Codom = Int}
  object arr2 extends Arrow {type Dom = Int; type Codom = Float}

  // removing "transparent" alleviates the situation
  inline transparent def compose[A, B](a: A, b: B)(using c: Composable[A,B]) = c.compose(a,b)
  val c = compose(arr2,arr1)