import language.experimental.captureChecking

class C1 extends caps.Control, caps.SharedCapability // OK

class C2 extends caps.Control, caps.Read // error

trait Async extends caps.Control
class Matrix extends caps.Read

class C3 extends Matrix, Async // error
