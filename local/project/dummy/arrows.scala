package dummy

import language.experimental.captureChecking
import language.experimental.pureFunctions
import caps.*

trait Nested:
  val c: AnyRef^
  val next: Nested

trait Arrows:
  val a: AnyRef^
  val b: AnyRef^
  val c: AnyRef^

  val purev: Int -> Int
  val purev2: Int ->{} Int
  val impurev: Int => Int
  val impurev2: Int ->{a,b,c} Int
  val impurev3: Int ->{a,b,c} Int => Int

  def pure(f: Int -> Int): Int
  def pure2(f: Int ->{} Int): Int
  def impure(f: Int => Int): Int
  def impure2(f: Int ->{a,b,c} Int): Int
  def impure3(f: Int ->{a,b,c} Int => Int): Int

  def uses(@use a: AnyRef^): Any
  def uses2(@use x: AnyRef^{a}, @use y: AnyRef^{b}): Any

  def consumes(@consume a: AnyRef^): Any
  def consumes2(@consume x: AnyRef^{a}, @consume y: AnyRef^{b}): Any

  def usesAndConsumes(@use a: AnyRef^, @consume b: AnyRef^): Any
  def usesAndConsumes2(@use @consume x: AnyRef^{a}): Any
  def consumesAndUses(@consume @use x: AnyRef^{a}): Any
  def consumesAndUses2(@consume @use x: List[AnyRef^]): Array[AnyRef^{x*}]

  def reachThis: AnyRef^{this*}

  def byNamePure(f: -> Int): Int
  def byNameImpure(f: ->{a,b,c} Int): Int
  def byNameImpure2(f: => Int): Int

  def pathDependent(n: Nested^)(g: AnyRef^{n.c} => Any): Any
  def pathDependent2(n: Nested^)(g: AnyRef^{n.next.c} => Any): Any
  def pathDependent3(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): Any
  def pathDependent4(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): AnyRef^{n.next.next.c}
  def pathDependent5(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): AnyRef^{n.next.next.c*, n.c}

  def contextPure(f: AnyRef^{a} ?-> Int): Int
  def contextImpure(f: AnyRef^{a} ?=> Int): Int
  def contextImpure2(f: AnyRef^{a} ?->{b,c} Int): Int
  def contextImpure3(f: AnyRef^{a} ?->{b,c} Int => AnyRef^{a} ?=> Int): Int