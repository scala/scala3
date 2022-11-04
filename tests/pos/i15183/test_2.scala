// Fails in each cases below
enum Env derives Decoder:
  case Local,Sit,Prod

enum Env2 derives Decoder:
  case Local()
  case Sit()
  case Prod()

enum Shape derives Decoder:
  case Rectangle(width: Double, height: Double)
  case Circle(radius: Double)
