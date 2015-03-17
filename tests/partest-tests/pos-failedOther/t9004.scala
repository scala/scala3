object Main {
  trait AA[RR] { type R = RR; def r: R }

  def test1(a: AA[_]) = {
    val f = () => a.r
    // The tree a.r is given the type `a.R` which normalizes
    // to B', where B' is a distinct symbol ("captured existential skolem")
    // to substitute for the reference to an existential skolem of B.
    //
    // inference of the result type of the function computes the
    // packed type of tree `a.r` to make sure that terms and types
    // local to the body of the function don't leak into its result
    // type. The captured existential skolem is considered to be local
    // so it is abstracted to its upper bound, Any.
    //
    // However, the packedType transformation need not have even considered
    // B', as it is clear that the type `a.R` is not local to the function
    // body!
    f: (() => a.R)

    // The workaround is to annotate the function type, rather than
    // relying in inference.
    val g: (() => a.R) = () => a.r
    val g2  = () => a.r

    ()
  }
  // typer debug trace: http://rawgit.com/retronym/d5aeaf8e0a4a2e6eef4b/raw/out.html
}
