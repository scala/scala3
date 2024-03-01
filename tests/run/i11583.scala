class Context:
  type Type
  type Term

class Env:
  type Extra

// TODO: enable after https://github.com/scala/scala3/issues/11700 is fixed
// extension [Ctx <: Context](using ctx: Ctx)(tpe: ctx.Type)(using env: Env)
//   /** essentially: `extension (s: String) def &&:(b: Boolean)(i: Int)`
//     * but exercises the RefinedPrinter and safety of reordering parameters
//     */
//   def &&:[T <: ctx.Term](trm: T)(ext: env.Extra): (ctx.Type, T, env.Extra) = (tpe, trm, ext)

extension [Ctx <: Context](using ctx: Ctx)(tpe: String)(using env: Env)
  def :#:[T <: Boolean](trm: T)(ext: env.Extra): (String, T, env.Extra) = (tpe, trm, ext)

extension [A](a: A)
  def :*:[T <: Tuple](t: T): A *: T = a *: t

@main def Test =

  given Context with
    type Type = String
    type Term = Boolean

  given Env with
    type Extra = Int

  val t1: (String, Boolean, Int) = true.:#:("hello")(23)
  // val t2: (String, Boolean, Int) = true.&&:("hello")(23)
  val t3: (String, Boolean, Int) = "hello" :*: (true, 23)
  val t4: (String, Boolean, Int) = (true, 23).:*:("hello")

  assert(t1 == ("hello", true, 23))
  // assert(t2 == ("hello", true, 23))
  assert(t3 == ("hello", true, 23))
  assert(t4 == ("hello", true, 23))
