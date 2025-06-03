// Fails in each cases below
import Decoder.{derived as _, given}
// NOTE Decoder.derived is already in the implicit scope
//  but the others require an import as they depend on match type reduction

enum Env derives Decoder:
  case Local,Sit,Prod

enum Env2 derives Decoder:
  case Local()
  case Sit()
  case Prod()

enum Shape derives Decoder:
  case Rectangle(width: Double, height: Double)
  case Circle(radius: Double)
