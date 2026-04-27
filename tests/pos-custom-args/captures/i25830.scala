import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

@main def test =
  val convert = { [C^] => (xs: List[File^{C}]) => xs.map(_ => ()) }
  val x = File()
  val files: List[File^{x}] = List(x)
  val result = convert[{x}](files)

  val convertCurried =
    { [C^] => (xs: List[File^{C}]) => (ys: List[File^{C}]) =>
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
