class Test {
  val x = '{0}
  val y = '{ // error: Canceled splice directly inside a quote. '{ ${ XYZ } } is equivalent to XYZ.
    $x
  }
  val z = '{
    val a = ${ // error: Canceled quote directly inside a splice. ${ '{ XYZ } } is equivalent to XYZ.
      '{
        $y
      }
    }
  }
}
