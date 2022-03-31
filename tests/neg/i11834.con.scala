//              || Put[Any] | Put[AnyRef] | Put[Z] | Put[Int] | Put[String] | Put[Null] | Put[Nothing] |
// ============ || ======== | =========== | ====== | ======== | =========== | ========= | ============ |
// Con[Any]     ||    --    |    error    | error  |  error   |    error    |   error   |    error     |
// Con[AnyRef]  ||    ok    |      -      | error  |  error   |    error    |   error   |    error     |
// Con[Z]       ||    ok    |    error    |   -    |  error   |    error    |   error   |    error     |
// Con[Int]     ||    ok    |    error    | error  |    -     |    error    |   error   |    error     |
// Con[String]  ||    ok    |     ok      | error  |  error   |      -      |   error   |    error     |
// Con[Null]    ||    ok    |     ok      | error  |  error   |     ok      |     -     |    error     |
// Con[Nothing] ||    ok    |     ok      |   ok   |    ok    |     ok      |    ok     |      -       |

trait Con[-X]
trait Put[-Y] extends Con[Y] { def put: Y => Boolean }

class PutAR    (val put: Any => Boolean)    extends Put[Any]    with Con[AnyRef]  // ok    // val x: Con[AnyRef]  = PutAR(_ == "");           if x.is[Put[AnyRef]]  then x.as[Put[AnyRef]].put("")
class PutAZ[-Z](val put: Any => Boolean)    extends Put[Any]    with Con[Z]       // ok    // val x: Con[String]  = PutAZ(_ == "");           if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutAI    (val put: Any => Boolean)    extends Put[Any]    with Con[Int]     // ok    // val x: Con[Int]     = PutAI(_ == 10);           if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutAS    (val put: Any => Boolean)    extends Put[Any]    with Con[String]  // ok    // val x: Con[String]  = PutAS(_ == ());           if x.is[Put[String]   then x.as[Put[String]].put("")
class PutAU    (val put: Any => Boolean)    extends Put[Any]    with Con[Null]    // ok    // val x: Con[Null]    = PutAU(_ == ());           if x.is[Put[Null]     then x.as[Put[Null]].put(null)
class PutAN    (val put: Any => Boolean)    extends Put[Any]    with Con[Nothing] // ok    // val x: Con[Nothing] = PutAN(_ == ());           if x.is[Put[Nothing]] then x.as[Put[Nothing]].put(???)

class PutRA    (val put: AnyRef => Boolean) extends Put[AnyRef] with Con[Any]     // error // val x: Con[Any]     = PutRA(_ == "");           if x.is[Put[Any]]     then x.as[Put[Any]].put(())
class PutRZ[-Z](val put: AnyRef => Boolean) extends Put[AnyRef] with Con[Z]       // error // val x: Con[String]  = PutRZ(_ == "");           if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutRI    (val put: AnyRef => Boolean) extends Put[AnyRef] with Con[Int]     // error // val x: Con[Int]     = PutRI(_ == 10);           if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutRS    (val put: AnyRef => Boolean) extends Put[AnyRef] with Con[String]  // ok    // val x: Con[String]  = PutRS(_ == ());           if x.is[Put[String]   then x.as[Put[String]].put("")
class PutRU    (val put: AnyRef => Boolean) extends Put[AnyRef] with Con[Null]    // ok    // val x: Con[Null]    = PutRU(_ == ());           if x.is[Put[Null]     then x.as[Put[Null]].put(null)
class PutRN    (val put: AnyRef => Boolean) extends Put[AnyRef] with Con[Nothing] // ok    // val x: Con[Nothing] = PutRN(_ == ());           if x.is[Put[Nothing]] then x.as[Put[Nothing]].put(???)

class PutZA[-Z](val put: Z => Boolean)      extends Put[Z]      with Con[Any]     // error // val x: Con[Any]     = PutZA[String](_.isEmpty); if x.is[Put[Any]]     then x.as[Put[Any]].put(())
class PutZR[-Z](val put: Z => Boolean)      extends Put[Z]      with Con[AnyRef]  // error // val x: Con[AnyRef]  = PutZR[String](_.isEmpty); if x.is[Put[AnyRef]]  then x.as[Put[AnyRef]].put("")
class PutZI[-Z](val put: Z => Boolean)      extends Put[Z]      with Con[Int]     // error // val x: Con[Int]     = PutZI[String](_.isEmpty); if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutZS[-Z](val put: Z => Boolean)      extends Put[Z]      with Con[String]  // error // val x: Con[String]  = PutZS[Int   ](_  == 1);   if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutZU[-Z](val put: Z => Boolean)      extends Put[Z]      with Con[Null]    // error // val x: Con[Null]    = PutZU[String](_.isEmpty); if x.is[Put[Null]]    then x.as[Put[Null]].put(???)
class PutZN[-Z](val put: Z => Boolean)      extends Put[Z]      with Con[Nothing] // ok    // val x: Con[Nothing] = PutZN[String](_.isEmpty); if x.is[Put[Nothing]] then x.as[Put[Nothing]].put(???)

class PutIA    (val put: Int => Boolean)    extends Put[Int]    with Con[Any]     // error // val x: Con[Any]     = PutIA(_ == 1);            if x.is[Put[Any]]     then x.as[Put[Any]].put(())
class PutIR    (val put: Int => Boolean)    extends Put[Int]    with Con[AnyRef]  // error // val x: Con[AnyRef]  = PutIR(_ == 1);            if x.is[Put[AnyRef]]  then x.as[Put[AnyRef]].put("")
class PutIZ[-Z](val put: Int => Boolean)    extends Put[Int]    with Con[Z]       // error // val x: Con[String]  = PutIZ(_ == 1);            if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutIS    (val put: Int => Boolean)    extends Put[Int]    with Con[String]  // error // val x: Con[String]  = PutZI(_ == 1);            if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutIU    (val put: Int => Boolean)    extends Put[Int]    with Con[Null]    // error // val x: Con[Null]    = PutIU(_ == 1);            if x.is[Put[Null]]    then x.as[Put[Null]].put(null)
class PutIN    (val put: Int => Boolean)    extends Put[Int]    with Con[Nothing] // ok    // val x: Con[Nothing] = PutIN(_ == 1);            if x.is[Put[Nothing]] then x.as[Put[Nothing]].put(???)

class PutSA    (val put: String => Boolean) extends Put[String] with Con[Any]     // error // val x: Con[Any]     = PutSA(_.isEmpty);         if x.is[Put[Any]]     then x.as[Put[Any]].put(())
class PutSR    (val put: String => Boolean) extends Put[String] with Con[AnyRef]  // error // val x: Con[AnyRef]  = PutSR(_.isEmpty);         if x.is[Put[AnyRef]]  then x.as[Put[AnyRef]].put("")
class PutSZ[-Z](val put: String => Boolean) extends Put[String] with Con[Z]       // error // val x: Con[Int]     = PutSZ(_.isEmpty);         if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutSI    (val put: String => Boolean) extends Put[String] with Con[Int]     // error // val x: Con[Int]     = PutSI(_.isEmpty);         if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutSU    (val put: String => Boolean) extends Put[String] with Con[Null]    // ok    // val x: Con[Null]    = PutSU(_.isEmpty);         if x.is[Put[Null]]    then x.as[Put[Null]].put(null)
class PutSN    (val put: String => Boolean) extends Put[String] with Con[Nothing] // ok    // val x: Con[Nothing] = PutSN(_.isEmpty);         if x.is[Put[Nothing]] then x.as[Put[Nothing]].put(???)

class PutUA    (val put: Null => Boolean)   extends Put[Null]   with Con[Any]     // error // val x: Con[Any]     = PutUA(_ == "");           if x.is[Put[Any]]     then x.as[Put[Any]].put(())
class PutUR    (val put: Null => Boolean)   extends Put[Null]   with Con[AnyRef]  // error // val x: Con[AnyRef]  = PutUR(_ == "");           if x.is[Put[AnyRef]   then x.as[Put[AnyRef]].put("")
class PutUZ[-Z](val put: Null => Boolean)   extends Put[Null]   with Con[Z]       // error // val x: Con[String]  = PutUZ(_ == "");           if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutUI    (val put: Null => Boolean)   extends Put[Null]   with Con[Int]     // error // val x: Con[Int]     = PutUI(_ == "");           if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutUS    (val put: Null => Boolean)   extends Put[Null]   with Con[String]  // error // val x: Con[String]  = PutUS(_ == "");           if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutUN    (val put: Null => Boolean)   extends Put[Null]   with Con[Nothing] // ok    // val x: Con[Nothing] = PutUN(_ == "");           if x.is[Put[Nothing]] then x.as[Put[Nothing]].put(???)

class PutNA                                 extends PutNothing  with Con[Any]     // error // val x: Con[Any]     = PutNA();                  if x.is[Put[Any]]     then x.as[Put[Any]].put(())
class PutNR                                 extends PutNothing  with Con[AnyRef]  // error // val x: Con[AnyRef]  = PutNA();                  if x.is[Put[AnyRef]   then x.as[Put[AnyRef]].put(())
class PutNZ[-Z]                             extends PutNothing  with Con[Z]       // error // val x: Con[String]  = PutNZ();                  if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutNI                                 extends PutNothing  with Con[Int]     // error // val x: Con[Int]     = PutNI();                  if x.is[Put[Int]]     then x.as[Put[Int]].put(10)
class PutNS                                 extends PutNothing  with Con[String]  // error // val x: Con[String]  = PutNI();                  if x.is[Put[String]]  then x.as[Put[String]].put("")
class PutNU                                 extends PutNothing  with Con[Null]    // error // val x: Con[Null]    = PutNI();                  if x.is[Put[Null]]    then x.as[Put[Null]].put(null)
class PutNothing extends Put[Nothing] { def put = (n: Nothing) => n }
