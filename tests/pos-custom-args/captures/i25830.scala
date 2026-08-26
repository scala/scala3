import language.experimental.captureChecking
import caps.*

class File

@main def test =
  val convert = { [C^] => (xs: List[File^{C}]) => xs.map(_ => ()) }
  val x: File^ = File()
  val files: List[File^{x}] = List(x)
  val result = convert[{x}](files)

  val globalFile: File^ = File()

  val convertCurried =
    { [C^] => (xs: List[File^{C}]) => (ys: List[File^{C}]) =>
        println(globalFile)
        xs.map(_ => ()) ++ ys.map(_ => ())
    }
  val resultCurried = convertCurried[{x}](files)(files)

  def convertDef =
    { [C^] => (xs: List[File^{C}]) => xs.map(_ => ()) }
  val resultDef = convertDef[{x}](files)

  val resultInAnonymousFunction =
    files.map: file =>
      val localFiles: List[File^{file}] = List(file)
      val localConvert =
        { [C^] => (xs: List[File^{C}]) => xs }
      localConvert[{file}](localFiles)

  // Poly-fn literal nested inside a Function1: fine as long as retains
  // only mention the literal's own capset binders.
  val nestedInFunction1 = (i: Int) => { [C^] => (xs: List[File^{C}]) => xs }
  val resultNested = nestedInFunction1(0)[{x}](files)

  // Capset binders interleaved with regular type binders.
  val interleaved1 = { [C^, A] => (xs: List[A]) => (ys: List[File^{C}]) => ys }
  val resultInterleaved1 = interleaved1[{x}, Int](List(1))(files)

  val interleaved2 = { [A, C^] => (xs: List[A]) => (ys: List[File^{C}]) => ys }
  val resultInterleaved2 = interleaved2[Int, {x}](List(1))(files)

  val interleaved3 =
    { [A, C^, B, D^] => (xs: List[A], ys: List[B]) =>
        (zs: List[File^{C}], ws: List[File^{D}]) => zs
    }
  val resultInterleaved3 = interleaved3[Int, {x}, String, {x}](List(1), List("a"))(files, files)

  // Multiple capset binder blocks separated by term-parameter lists.
  val multi1 =
    { [C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{D}]) => (xs, ys) }
  val resultMulti1 = multi1[{x}](files)[{x}](files)

  val multi2 =
    { [C^] => (xs: List[File^{C}]) => [A] => (zs: List[A]) => [D^] => (ws: List[File^{D}]) => (xs, zs, ws) }
  val resultMulti2 = multi2[{x}](files)[Int](List(1))[{x}](files)

  // Non-capset block first, then capset block.
  val multi3 = { [A] => (zs: List[A]) => [C^] => (xs: List[File^{C}]) => (zs, xs) }
  val resultMulti3 = multi3[Int](List(1))[{x}](files)

  // Inner block references both capset binders.
  val multi4 =
    { [C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{C, D}]) => ys }
  val resultMulti4 = multi4[{x}](files)[{x}](files)
