def zip[A, B](
  xs: List[A],
  ys: List[B] with ys.size == xs.size
): {res: List[(A, B)] with res.size == xs.size} =
  (xs, ys) match
    case (x :: xt, y :: yt) =>
      ((x, y) :: zip[A, B](xt, yt.runtimeChecked)).runtimeChecked
    case _ =>
      (Nil: List[(A, B)]).runtimeChecked
