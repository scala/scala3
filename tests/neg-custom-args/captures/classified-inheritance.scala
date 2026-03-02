import language.experimental.captureChecking

class C1 extends caps.Control, caps.SharedCapability // OK

class C2 extends caps.Control, caps.Unscoped // error

trait Async extends caps.Control
class Matrix extends caps.Unscoped

class C3 extends Matrix, Async // error
