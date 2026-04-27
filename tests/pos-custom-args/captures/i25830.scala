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
