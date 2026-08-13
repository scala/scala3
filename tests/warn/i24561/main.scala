//> using options -Wunused:all
import ext.Reader // warn
import ext.NonEmptyList // warn

@main def run = locally {
  macros.demo
}
