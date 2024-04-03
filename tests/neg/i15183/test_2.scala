// Fails in each cases below
enum Env derives Decoder:  // error
  case Local,Sit,Prod

enum Env2 derives Decoder: // error
  case Local()
  case Sit()
  case Prod()

enum Shape derives Decoder: // error
  case Rectangle(width: Double, height: Double)
  case Circle(radius: Double)
