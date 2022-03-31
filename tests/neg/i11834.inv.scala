class Animal
class Cat extends Animal
class Dog extends Animal

trait Cov[+X]
trait Con[-X]
trait Mod[Z] extends AnyRef with Cov[Z] with Con[Z] { def mod: Z => Z }

class ModCovCD(val mod: Cat => Cat) extends Mod[Cat] with Cov[Dog] // error
class ModCovDC(val mod: Dog => Dog) extends Mod[Dog] with Cov[Cat] // error
class ModConCD(val mod: Cat => Cat) extends Mod[Cat] with Con[Dog] // error
class ModConDC(val mod: Dog => Dog) extends Mod[Dog] with Con[Cat] // error
