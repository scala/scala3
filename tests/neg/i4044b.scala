import scala.quoted._

class Test {

  '{

    val b = '{3}

    '{
      b // error
      ${b}
      ${ '{b} } // error
      '{ '{$b} } // error
    }

  }

}
