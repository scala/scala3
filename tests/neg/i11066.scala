class PreferredPrompt(val preference: String)
object Greeter:
  def greet(name: String)(using prompt: PreferredPrompt) =
    println(s"Welcome, $name. The system is ready.")
    println(prompt.preference)
object JillsPrefs:
  given jillsPrompt: PreferredPrompt =
    PreferredPrompt("Your wish> ")
object JoesPrefs:
  given joesPrompt: PreferredPrompt =
    PreferredPrompt("relax> ")

import JillsPrefs.jillsPrompt
import JoesPrefs.joesPrompt
val x = Greeter.greet("Who's there?") // error
