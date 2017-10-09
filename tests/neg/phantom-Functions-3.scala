
// TODO: Importing an object that exends phantom makes FunctionN refere to the Boo.FunctionN
//       We should be carefull with this. Use a waring when importing Boo._ or disallow it.
//       Or put funtions in an inner object in the Phantom trait, for example scala.Phantom.Functions

import Boo._

class Fun extends Function1[Int, Unit] { // error: Type argument Int does not conform to upper bound Boo.Any
  def apply(x1: Int): Unit = ()
}

object Boo extends Phantom {
  type Casper <: this.Any
}
