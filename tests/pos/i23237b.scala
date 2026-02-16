object types{
  final case class vec2(x: Float, y: Float)
  final case class vec4(x: Float, y: Float, z: Float, w: Float)
  object vec4:
    def apply(xy: vec2, z: Float, w: Float): vec4 = vec4(xy.x, xy.y, z, w)

  opaque type Shader[In, Out] = In => Out
  object Shader:
    inline def apply[In, Out](f: In => Out): Shader[In, Out] = f
    inline def apply[In](f: In => Unit): Shader[In, Unit]    = f
    inline def apply(body: => Any): Shader[Unit, Unit] = (_: Unit) => body
}
import types.*

class GLSLEnvTests {
  case class FragEnv(UV: vec2)

  inline def fragment: Shader[FragEnv, vec4] =
    Shader { env =>
      val x = env.UV
      vec4(env.UV, 0.0f, 1.0f)
    }
}
