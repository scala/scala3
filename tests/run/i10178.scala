@main def Test: Unit =
  for
    x <- Option(23)
    given Int = x
  do assert(summon[Int] == 23)

  for
    y <- Option("ok")
    q @ given String = y
  do assert(summon[String] == "ok")

  for
    z <- Option("key" -> true)
    (q @ given String, u @ given Boolean) = z
  do
    assert(summon[String] == "key")
    assert(summon[Boolean] == true)

  for
    w <- Option("no" -> false)
    (given String, given Boolean) = w
  do
    assert(summon[String] == "no")
    assert(summon[Boolean] == false)

  for
    given Int <- Option(23)
  do assert(summon[Int] == 23)

  for
    q @ given String <- Option("ok")
  do assert(summon[String] == "ok")

  for
    (q @ given String, u @ given Boolean) <- Option("key" -> true)
  do
    assert(summon[String] == "key")
    assert(summon[Boolean] == true)

  for
    (given String, given Boolean) <- Option("no" -> false)
  do
    assert(summon[String] == "no")
    assert(summon[Boolean] == false)
