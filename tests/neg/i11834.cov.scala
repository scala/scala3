//              || Get[Any] | Get[AnyRef] | Get[Z] | Get[Int] | Get[String] | Get[Null] | Get[Nothing] |
// ============ || ======== | =========== | ====== | ======== | =========== | ========= | ============ |
// Cov[Any]     ||    -     |     ok      |   ok   |    ok    |     ok      |    ok     |      ok      |
// Cov[AnyRef]  ||  error   |      -      | error  |  error   |     ok      |    ok     |      ok      |
// Cov[Z]       ||  error   |    error    |   -    |  error   |    error    |   error   |      ok      |
// Cov[Int]     ||  error   |    error    | error  |    -     |    error    |   error   |      ok      |
// Cov[String]  ||  error   |    error    | error  |  error   |      -      |    ok     |      ok      |
// Cov[Null]    ||  error   |    error    | error  |  error   |    error    |     -     |      ok      |
// Cov[Nothing] ||  error   |    eoror    | error  |  error   |    error    |   error   |      --      |

trait Cov[+X]
trait Get[+Y] extends Cov[Y] { def get: Y }

class GetAR    (val get: Any)    extends Get[Any]    with Cov[AnyRef]  // error // val x: Cov[AnyRef]  = GetAR(10);   if x.is[Get[AnyRef]]  then x.as[Get[AnyRef]].get
class GetAZ[+Z](val get: Any)    extends Get[Any]    with Cov[Z]       // error // val x: Cov[Int]     = GetAZ(());   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetAI    (val get: Any)    extends Get[Any]    with Cov[Int]     // error // val x: Cov[Int]     = GetAI(());   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetAS    (val get: Any)    extends Get[Any]    with Cov[String]  // error // val x: Cov[String]  = GetAI(());   if x.is[Get[String]]  then x.as[Get[String]].get
class GetAU    (val get: Any)    extends Get[Any]    with Cov[Null]    // error // val x: Cov[Null]    = GetAU(());   if x.is[Get[Null]]    then x.as[Get[Null]].get
class GetAN    (val get: Any)    extends Get[Any]    with Cov[Nothing] // error // val x: Cov[Nothing] = GetAN(());   if x.is[Get[Nothing]] then x.as[Get[Nothing]].get

class GetRA    (val get: AnyRef) extends Get[AnyRef] with Cov[Any]     // ok    // val x: Cov[Any]     = GetRA(10);   if x.is[Get[Any]]     then x.as[Get[Any]].get
class GetRZ[+Z](val get: AnyRef) extends Get[AnyRef] with Cov[Z]       // error // val x: Cov[Int]     = GetRZ(());   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetRI    (val get: AnyRef) extends Get[AnyRef] with Cov[Int]     // error // val x: Cov[Int]     = GetRI(());   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetRS    (val get: AnyRef) extends Get[AnyRef] with Cov[String]  // error // val x: Cov[String]  = GetRS(());   if x.is[Get[String]]  then x.as[Get[String]].get
class GetRU    (val get: AnyRef) extends Get[AnyRef] with Cov[Null]    // error // val x: Cov[Null]    = GetRU(());   if x.is[Get[Null]]    then x.as[Get[Null]].get
class GetRN    (val get: AnyRef) extends Get[AnyRef] with Cov[Nothing] // error // val x: Cov[Nothing] = GetRN(());   if x.is[Get[Nothing]] then x.as[Get[Nothing]].get

class GetZA[+Z](val get: Z)      extends Get[Z]      with Cov[Any]     // ok    // val x: Cov[Any]     = GetZA(());   if x.is[Get[Any]]     then x.as[Get[Any]].get
class GetZR[+Z](val get: Z)      extends Get[Z]      with Cov[AnyRef]  // error // val x: Cov[AnyRef]  = GetZR(());   if x.is[Get[AnyRef]]  then x.as[Get[AnyRef]].get
class GetZI[+Z](val get: Z)      extends Get[Z]      with Cov[Int]     // error // val x: Cov[Int]     = GetZI(());   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetZS[+Z](val get: Z)      extends Get[Z]      with Cov[String]  // error // val x: Cov[String]  = GetZS(());   if x.is[Get[String]]  then x.as[Get[String]].get
class GetZU[+Z](val get: Z)      extends Get[Z]      with Cov[Null]    // error // val x: Cov[Null]    = GetZU(());   if x.is[Get[Null]]    then x.as[Get[Null]].get
class GetZN[+Z](val get: Z)      extends Get[Z]      with Cov[Nothing] // error // val x: Cov[Nothing] = GetZN(());   if x.is[Get[Nothing]] then x.as[Get[Nothing]].get

class GetIA    (val get: Int)    extends Get[Int]    with Cov[Any]     // ok    // val x: Cov[Any]     = GetIA(10);   if x.is[Get[Any]]     then x.as[Get[Any]].get
class GetIR    (val get: Int)    extends Get[Int]    with Cov[AnyRef]  // error // val x: Cov[AnyRef]  = GetIR(10);   if x.is[Get[AnyRef]]  then x.as[Get[AnyRef]].get
class GetIZ[+Z](val get: Int)    extends Get[Int]    with Cov[Z]       // error // val x: Cov[String]  = GetIZ(10);   if x.is[Get[String]]  then x.as[Get[String]].get
class GetIS    (val get: Int)    extends Get[Int]    with Cov[String]  // error // val x: Cov[String]  = GetIS(10);   if x.is[Get[String]]  then x.as[Get[String]].get
class GetIU    (val get: Int)    extends Get[Int]    with Cov[Null]    // error // val x: Cov[Null]    = GetIU(10);   if x.is[Get[Null]]    then x.as[Get[Null]].get
class GetIN    (val get: Int)    extends Get[Int]    with Cov[Nothing] // error // val x: Cov[Nothing] = GetIN(10);   if x.is[Get[Nothing]] then x.as[Get[Nothing]].get

class GetSA    (val get: String) extends Get[String] with Cov[Any]     // ok    // val x: Cov[Any]     = GetSA("");   if x.is[Get[Any]]     then x.as[Get[Any]].get
class GetSR    (val get: String) extends Get[String] with Cov[AnyRef]  // ok    // val x: Cov[AnyRef]  = GetSR("");   if x.is[Get[AnyRef]]  then x.as[Get[AnyRef]].get
class GetSZ[+Z](val get: String) extends Get[String] with Cov[Z]       // error // val x: Cov[Int]     = GetSZ("");   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetSI    (val get: String) extends Get[String] with Cov[Int]     // error // val x: Cov[Int]     = GetSI("");   if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetSU    (val get: String) extends Get[String] with Cov[Null]    // error // val x: Cov[Null]    = GetSU("");   if x.is[Get[Null]]    then x.as[Get[Null]].get
class GetSN    (val get: String) extends Get[String] with Cov[Nothing] // error // val x: Cov[Nothing] = GetSN("");   if x.is[Get[Nothing]] then x.as[Get[Nothing]].get

class GetUA    (val get: Null)   extends Get[Null]   with Cov[Any]     // ok    // val x: Cov[Any]     = GetUA(null); if x.is[Get[Any]]     then x.as[Get[Any]].get
class GetUR    (val get: Null)   extends Get[Null]   with Cov[AnyRef]  // ok    // val x: Cov[AnyRef]  = GetUR(null); if x.is[Get[AnyRef]]  then x.as[Get[AnyRef]].get
class GetUZ[+Z](val get: Null)   extends Get[Null]   with Cov[Z]       // error // val x: Cov[String]  = GetUZ(null); if x.is[Get[String]]  then x.as[Get[String]].get
class GetUI    (val get: Null)   extends Get[Null]   with Cov[Int]     // error // val x: Cov[Int]     = GetUI(null); if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetUS    (val get: Null)   extends Get[Null]   with Cov[String]  // ok    // val x: Cov[String]  = GetUI(null); if x.is[Get[String]]  then x.as[Get[String]].get
class GetUN    (val get: Null)   extends Get[Null]   with Cov[Nothing] // error // val x: Cov[Nothing] = GetUN(null); if x.is[Get[Nothing]] then x.as[Get[Nothing]].get

class GetNA                      extends GetNothing  with Cov[Any]     // ok    // val x: Cov[Any]    = GetNA();  if x.is[Get[Any]]     then x.as[Get[Any]].get
class GetNR                      extends GetNothing  with Cov[AnyRef]  // ok    // val x: Cov[AnyRef] = GetNR();  if x.is[Get[AnyRef]]  then x.as[Get[AnyRef]].get
class GetNZ[+Z]                  extends GetNothing  with Cov[Z]       // ok    // val x: Cov[String] = GetNZ();  if x.is[Get[String]]  then x.as[Get[String]].get
class GetNI                      extends GetNothing  with Cov[Int]     // ok    // val x: Cov[Int]    = GetNI();  if x.is[Get[Int]]     then x.as[Get[Int]].get
class GetNS                      extends GetNothing  with Cov[String]  // ok    // val x: Cov[String] = GetNI();  if x.is[Get[String]]  then x.as[Get[String]].get
class GetNU                      extends GetNothing  with Cov[Null]    // ok    // val x: Cov[Null]   = GetNU();  if x.is[Get[Null]]    then x.as[Get[Null]].get
class GetNothing extends Get[Nothing] { def get = throw new AssertionError(s"I am ${getClass.getSimpleName}") }
