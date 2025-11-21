import scala.annotation.valhalla

trait RecordLabel:
  var name: String = "Sony"

@valhalla trait SoundProduction extends Any { // error
  this: RecordLabel =>
    val prodName = "prod " + name
}