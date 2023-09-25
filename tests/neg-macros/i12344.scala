import scala.quoted.*

class C(using q: Quotes)(i: Int = 1, f: q.reflect.Flags = q.reflect.Flags.EmptyFlags)

def test1a(using q: Quotes) = new C()
def test2a(using q: Quotes) = new C(1)
def test3a(using q: Quotes) = new C(1, q.reflect.Flags.Lazy)
def test4a(using q: Quotes) = new C(f = q.reflect.Flags.Lazy)

def test1b(using q: Quotes) = C()
def test2b(using q: Quotes) = C(1)
def test3b(using q: Quotes) = C(1, q.reflect.Flags.Lazy)
def test4b(using q: Quotes) = C(f = q.reflect.Flags.Lazy)

def test1c(using q: Quotes) = new C(using q)()
def test2c(using q: Quotes) = new C(using q)(1)
def test3c(using q: Quotes) = new C(using q)(1, q.reflect.Flags.Lazy)
def test4c(using q: Quotes) = new C(using q)(f = q.reflect.Flags.Lazy)

def test1d(using q: Quotes) = C(using q)()
def test2d(using q: Quotes) = C(using q)(1)
def test3d(using q: Quotes) = C(using q)(1, q.reflect.Flags.Lazy)
def test4d(using q: Quotes) = C(using q)(f = q.reflect.Flags.Lazy)

def test1e(using q: Quotes) = new C()()  // error
def test2e(using q: Quotes) = C()()      // error
