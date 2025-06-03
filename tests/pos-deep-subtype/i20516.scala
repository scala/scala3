object Main {
  trait A {}
  trait B {}
  trait C {}
  trait D {}
  trait E {}
  trait F {}
  trait G {}
  trait H {}
  trait I {}
  trait J {}
  trait K {}
  trait L {}
  trait M {}
  trait N {}
  trait O {}
  trait P {}
  trait Q {}
  trait R {}
  trait S {}
  trait T {}
  trait U {}
  trait V {}
  trait W {}
  trait X {}
  trait Y {}
  trait Z {}

  type AlphabeticServices = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z

  type EnvOutA = B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutB = A & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutC = A & B & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutD = A & B & C & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutE = A & B & C & D & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutF = A & B & C & D & E & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutG = A & B & C & D & E & F & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutH = A & B & C & D & E & F & G & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutI = A & B & C & D & E & F & G & H & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutJ = A & B & C & D & E & F & G & H & I & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutK = A & B & C & D & E & F & G & H & I & J & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutL = A & B & C & D & E & F & G & H & I & J & K & M & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutM = A & B & C & D & E & F & G & H & I & J & K & L & N & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutN = A & B & C & D & E & F & G & H & I & J & K & L & M & O & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutO = A & B & C & D & E & F & G & H & I & J & K & L & M & N & P & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutP = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & Q & R & S & T & U & V & W & X & Y & Z
  type EnvOutQ = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & R & S & T & U & V & W & X & Y & Z
  type EnvOutR = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & S & T & U & V & W & X & Y & Z
  type EnvOutS = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & T & U & V & W & X & Y & Z
  type EnvOutT = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & U & V & W & X & Y & Z
  type EnvOutU = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & V & W & X & Y & Z
  type EnvOutV = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & W & X & Y & Z
  type EnvOutW = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & X & Y & Z
  type EnvOutX = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & Y & Z
  type EnvOutY = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Z
  type EnvOutZ = A & B & C & D & E & F & G & H & I & J & K & L & M & N & O & P & Q & R & S & T & U & V & W & X & Y

  trait Reader[-E, A] {
    def map[B](f: A => B): Reader[E, B] = ???
    def flatMap[E2 <: E, B](f: A => Reader[E2, B]): Reader[E2, B] = ???
  }

  def e1: Reader[EnvOutA, Unit] = ???
  def e2: Reader[EnvOutB, Unit] = ???
  def e3: Reader[EnvOutC, Unit] = ???
  def e4: Reader[EnvOutD, Unit] = ???
  def e5: Reader[EnvOutE, Unit] = ???
  def e6: Reader[EnvOutF, Unit] = ???
  def e7: Reader[EnvOutG, Unit] = ???
  def e8: Reader[EnvOutH, Unit] = ???
  def e9: Reader[EnvOutI, Unit] = ???
  def e10: Reader[EnvOutJ, Unit] = ???
  def e11: Reader[EnvOutK, Unit] = ???
  def e12: Reader[EnvOutL, Unit] = ???
  def e13: Reader[EnvOutM, Unit] = ???
  def e14: Reader[EnvOutN, Unit] = ???
  def e15: Reader[EnvOutO, Unit] = ???
  def e16: Reader[EnvOutP, Unit] = ???
  def e17: Reader[EnvOutQ, Unit] = ???
  def e18: Reader[EnvOutR, Unit] = ???
  def e19: Reader[EnvOutS, Unit] = ???
  def e20: Reader[EnvOutT, Unit] = ???
  def e21: Reader[EnvOutU, Unit] = ???
  def e22: Reader[EnvOutV, Unit] = ???
  def e23: Reader[EnvOutW, Unit] = ???
  def e24: Reader[EnvOutX, Unit] = ???
  def e25: Reader[EnvOutY, Unit] = ???
  def e26: Reader[EnvOutZ, Unit] = ???

  def program: Reader[AlphabeticServices, Unit] = for {
    //1
    _ <- e1
    _ <- e2
    _ <- e3
    _ <- e4
    _ <- e5
    _ <- e6
    _ <- e7
    _ <- e8
    _ <- e8
    _ <- e9
    _ <- e10
    _ <- e11
    _ <- e12
    _ <- e13
    _ <- e14
    _ <- e15
    _ <- e16
    _ <- e17
    _ <- e18
    _ <- e19
    _ <- e20
    _ <- e21
    _ <- e22
    _ <- e23
    _ <- e24
    _ <- e25
    _ <- e26
    // 2
    _ <- e1
    _ <- e2
    _ <- e3
    _ <- e4
    _ <- e5
    _ <- e6
    _ <- e7
    _ <- e8
    _ <- e8
    _ <- e9
    _ <- e10
    _ <- e11
    _ <- e12
    _ <- e13
    _ <- e14
    _ <- e15
    _ <- e16
    _ <- e17
    _ <- e18
    _ <- e19
    _ <- e20
    _ <- e21
    _ <- e22
    _ <- e23
    _ <- e24
    _ <- e25
    _ <- e26
    // TODO: optimize the subtype checking for large intersection types further
    //3
    // _ <- e1
    // _ <- e2
    // _ <- e3
    // _ <- e4
    // _ <- e5
    // _ <- e6
    // _ <- e7
    // _ <- e8
    // _ <- e8
    // _ <- e9
    // _ <- e10
    // _ <- e11
    // _ <- e12
    // _ <- e13
    // _ <- e14
    // _ <- e15
    // _ <- e16
    // _ <- e17
    // _ <- e18
    // _ <- e19
    // _ <- e20
    // _ <- e21
    // _ <- e22
    // _ <- e23
    // _ <- e24
    // _ <- e25
    // _ <- e26
    // 4
    // _ <- e1
    // _ <- e2
    // _ <- e3
    // _ <- e4
    // _ <- e5
    // _ <- e6
    // _ <- e7
    // _ <- e8
    // _ <- e8
    // _ <- e9
    // _ <- e10
    // _ <- e11
    // _ <- e12
    // _ <- e13
    // _ <- e14
    // _ <- e15
    // _ <- e16
    // _ <- e17
    // _ <- e18
    // _ <- e19
    // _ <- e20
    // _ <- e21
    // _ <- e22
    // _ <- e23
    // _ <- e24
    // _ <- e25
    // _ <- e26
    } yield ()
}