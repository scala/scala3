object Test {
  type Obj
  type Forall[F[_]] = (x: Obj) => F[x.type]

  trait EQ[A, B] {
    def sub[F[_]]: F[A] => F[B];
    def commute: EQ[B, A] = this.sub[[b] =>> EQ[b, A]](implicitly[EQ[A, A]])
  }
  implicit def typeEq[A]: EQ[A, A] = new EQ[A, A] {
    def sub[F[_]]: F[A] => F[A] = identity
  }

  // these are both fine
  val eqReflexive1: (x: Obj) => (EQ[x.type, x.type]) = { x: Obj => implicitly }
  val eqReflexive2: Forall[[x] =>> EQ[x, x]] = { x: Obj => implicitly }

  // this compiles
  val eqSymmetric1: (x: Obj) => (y: Obj) => EQ[x.type, y.type] => EQ[y.type, x.type] = {
    { x: Obj => { y: Obj => { xEqy: EQ[x.type, y.type] => xEqy.commute } } }
  }

  val eqSymmetric2: Forall[[x] =>> (y: Obj) => (EQ[x, y.type]) => (EQ[y.type, x])] = {
    { x: Obj => { y: Obj => { xEqy: EQ[x.type, y.type] => xEqy.commute } } } // error // error
  }

  val eqSymmetric3: Forall[[x] =>> Forall[[y] =>> EQ[x, y] => EQ[y, x]]] = {
    { x: Obj => { y: Obj => { xEqy: EQ[x.type, y.type] => xEqy.commute } } } // error // error
  }
}