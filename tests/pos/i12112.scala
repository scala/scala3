object A:
  object B:
    object C

object X {
  import A.B

  B.C        // ok
  export B.C // error
}
