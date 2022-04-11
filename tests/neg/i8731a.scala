object test:
  while
    3 == 3
  end while  // error: `do` expected
  do ()

  for
    a <- Seq()
  end for    // error: `yield` or `do` expected
  do ()