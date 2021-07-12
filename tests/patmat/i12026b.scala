def test[A, B](a: A|B)(tta: reflect.TypeTest[Any, A], ttb: reflect.TypeTest[Any, B]) =
  a match {
    case tta(a: A) =>
    case ttb(b: B) =>
  }
