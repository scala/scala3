enum Enum:
  case A
  case B
  case C()
  case D()
  case E(x: Int)

@main def Test =
  // Enum values (were not stable from run to run before #23218)
  println(Enum.A.hashCode)
  println(Enum.A.hashCode)
  println(Enum.B.hashCode)
  println(Enum.B.hashCode)

  // Other enum cases (were already stable from run to run)
  println(Enum.C().hashCode)
  println(Enum.C().hashCode)
  println(Enum.D().hashCode)
  println(Enum.D().hashCode)
  println(Enum.E(1).hashCode)
  println(Enum.E(1).hashCode)
  println(Enum.E(2).hashCode)
  println(Enum.E(2).hashCode)
