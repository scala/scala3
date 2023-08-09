@main def main: Unit =
  type T = 3f
  val value0: T = -3.5f // error
  val value1: T = -100500 // error
  val value2: T = -100500L // error
  val value3: T = -100500D // error
  val value4: T = true // error
  val value5: 3f = -100500 // error
  val value6: 3f = -100500L // error

  type Ti = 3
  val value1i: Ti = -100500  // error
  val value2i: Ti = -100500L // error
  val value0i: Ti = -100500F // error
  val value3i: Ti = -100500D // error
  val value4i: Ti = true     // error
  val value5i: 3  = -100500  // error
  val value6i: 3  = -100500L // error

  type Tl = 3L
  val value1l: Tl = -100500  // error
  val value2l: Tl = -100500L // error
  val value0l: Tl = -100500F // error
  val value3l: Tl = -100500D // error
  val value4l: Tl = true     // error
  val value5l: 3L = -100500  // error
  val value6l: 3L = -100500L // error

  type Td = 3D
  val value1d: Td = -100500  // error
  val value2d: Td = -100500L // error
  val value0d: Td = -100500F // error
  val value3d: Td = -100500D // error
  val value4d: Td = true     // error
  val value5d: 3D = -100500  // error
  val value6d: 3D = -100500L // error
