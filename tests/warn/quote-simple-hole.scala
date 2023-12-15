

import scala.quoted.Quotes

def test(using Quotes) = {
  val x = '{0}
  val y = '{ // warn: Canceled splice directly inside a quote. '{ ${ XYZ } } is equivalent to XYZ.
    $x
  }
  val z = '{
    val a = ${ // warn: Canceled quote directly inside a splice. ${ '{ XYZ } } is equivalent to XYZ.
      '{
        $y
      }
    }
  }
}