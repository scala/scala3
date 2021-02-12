import quoted.*

object TestImpl {
  transparent inline def fun (inline arg: String): String =
    /*
    Pad out the file
    Lorem ipsum dolor sit amet consectetur adipiscing elit tincidunt id augue, facilisi dignissim nibh litora lectus quam senectus fringilla molestie sollicitudin, nunc ullamcorper auctor turpis integer netus fermentum mattis magna. Nullam potenti diam tellus bibendum odio tristique felis, pharetra posuere at imperdiet suspendisse aenean, eu lobortis sapien eleifend aptent sociosqu.
    */
    ${ CodeImpl.codeExpr }
}
