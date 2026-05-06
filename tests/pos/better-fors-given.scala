@main def Test: Unit =
  for
    x <- Option(23 -> "abc")
    (a @ given Int, b @ given String) = x
    _ <- Option(1)
  yield
    assert(summon[Int] == 23)

  for
    x <- Option((1.3, 23 -> "abc"))
    (_, (a @ given Int, b @ given String)) = x
    _ <- Option(1)
  yield
    assert(summon[Int] == 23)

  for
    x <- Option(Some(23 -> "abc"))
    Some(a @ given Int, b @ given String) = x
    _ <- Option(1)
  yield
    assert(summon[Int] == 23)

  for
    x <- Option(Some(23))
    Some(a @ given Int) = x
    _ <- Option(1)
  yield
    assert(summon[Int] == 23)

  for
    x <- Option(23)
    a @ given Int = x
  yield
    assert(summon[Int] == 23)

  for
    x <- Option(23)
    _ @ given Int = x
  yield
    assert(summon[Int] == 23)

  for
    x <- Option(23)
    given Int = x
  yield
    assert(summon[Int] == 23)

  for
    x <- Option(23)
    given Int = x
    _ <- Option(1)
  yield
    assert(summon[Int] == 23)

  for
    a @ given Int <- Option(23)
  yield
    assert(summon[Int] == 23)

  for
    _ @ given Int <- Option(23)
  yield
    assert(summon[Int] == 23)

  for
    given Int <- Option(23)
  yield
    assert(summon[Int] == 23)