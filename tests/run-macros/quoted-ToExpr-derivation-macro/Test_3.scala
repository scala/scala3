object Test extends App {
  {
    import LibA._
    assert(optTwo == Sm(2))
    assert(smTwo == Sm(2))
    assert(none == Nn)
  }

  {
    import LibB._
    assert(optTwo == Sm(2))
    assert(smTwo == Sm(2))
    assert(none == Nn)
  }


  {
    import LibC._
    import Opt._
    assert(optTwo == Sm(2))
    assert(smTwo == Sm(2))
    assert(none == Nn)
  }
}
