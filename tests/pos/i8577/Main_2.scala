package i8577

def main: Unit = {
  {
    extension (ctx: StringContext) def macroA: MacroB.StringContext = MacroB(ctx)
    extension (inline ctx: MacroB.StringContext) inline def unapplySeq(inline input: Int): Option[Seq[Int]] =
      ${implUnapplyA('ctx, 'input)}

    val macroA"$xA" = 1
  }

  {
    extension (ctx: StringContext) def macroB: MacroB.StringContext = MacroB(ctx)
    extension (inline ctx: MacroB.StringContext) inline def unapplySeq[U](inline input: U): Option[Seq[U]] =
      ${ implUnapplyB('ctx, 'input) }

    val macroB"$xB" = 2
  }

  {
    extension (ctx: StringContext) def macroC: MacroC.StringContext = MacroC(ctx)
    extension [T] (inline ctx: MacroC.StringContext) inline def unapplySeq(inline input: T): Option[Seq[T]] =
      ${ implUnapplyC('ctx, 'input) }

    val macroC"$xC" = 3
  }

  {
    extension (ctx: StringContext) def macroD: MacroD.StringContext = MacroD(ctx)
    extension [T] (inline ctx: MacroD.StringContext) inline def unapplySeq[U](inline input: T): Option[Seq[T]] =
      ${ implUnapplyD('ctx, 'input) }

    // miscompilation
//    val macroD"$xD" = 4
  }

  {
    extension (ctx: StringContext) def macroE: MacroE.StringContext = MacroE(ctx)
    extension [T] (inline ctx: MacroE.StringContext) inline def unapplySeq[U](inline input: U): Option[Seq[U]] =
      ${ implUnapplyE('ctx, 'input) }

    val macroE"$xE" = 5
  }

  {
    extension (ctx: StringContext) def macroF: MacroF.StringContext = MacroF(ctx)
    extension [T] (inline ctx: MacroF.StringContext) inline def unapplySeq[U](inline input: (T, U)): Option[Seq[(T, U)]] =
      ${ implUnapplyF('ctx, 'input) }

    val macroF"$xF" = (6, 7)

    // miscompilation
//    val macroF"$xF" = (6, "7")
  }

  {
    extension (ctx: StringContext) def macroG: MacroG.StringContext = MacroG(ctx)
    extension [T] (inline ctx: MacroG.StringContext) inline def unapplySeq[U](inline input: T | U): Option[Seq[T | U]] =
      ${ implUnapplyG('ctx, 'input) }

    // compiler error
//    val macroG"$xG" = 8
  }
}

//  {
//    // B
//    object F2
//    extension (o: F2.type) inline def unapplySeq[T](inline x: T) = Some(Seq(x))
//
//    val F2(x) = 16
//    println(s"F2: $x")
//  }
//
//  {
//    // C
//    object F1
//    extension [T] (o: F1.type) inline def unapplySeq(inline x: T) = Some(Seq(x))
//
//    val F1(x) = 15
//    println(s"F1: $x")
//  }
//
//  {
//    // D
//    object F4b
//    extension [T] (o: F4b.type) inline def unapplySeq[U](inline x: T) = Some(Seq(x))
//
//    val F4b(x) = 18.2
//    println(s"F4b: $x")
//  }
//
//  {
//    // E
//    object F4b
//    extension [T] (o: F4b.type) inline def unapplySeq[U](inline x: U) = Some(Seq(x))
//
//    val F4b(x) = 18.2
//    println(s"F4b: $x")
//  }
//
//  {
//    // F
//    object F4d
//    extension [T] (o: F4d.type) inline def unapplySeq[U](inline x: (T, U)) = Some(Seq(x))
//
//    val F4d(x) = (18.4, 18.5)
//    println(s"F4d: $x")
//  }
//
//  {
//    // G
//    object H1
//    extension (inline o: H1.type) inline def unapplySeq(inline x: Int) = Some(Seq(x))
//
//    val H1(x) = 23
//    println(s"H1: $x")
//  }
//
//  {
//    // H
//    object H2
//    extension (inline o: H2.type) inline def unapplySeq[T](inline x: T) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // I
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq(inline x: T) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // J
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq[U](inline x: T) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // K
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq[U](inline x: U) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // L
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq[U](inline x: (T, U)) = Some(Seq(x))
//
//    val H2(x) = (24, "a")
//    println(s"H2: $x")
//  }
//}
