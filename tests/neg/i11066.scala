class PreferredPrompt(val preference: String)
object Greeter with
  def greet(name: String)(using prompt: PreferredPrompt) =
    println(s"Welcome, $name. The system is ready.")
    println(prompt.preference)
object JillsPrefs with
  given jillsPrompt: PreferredPrompt =
    PreferredPrompt("Your wish> ")
object JoesPrefs with
  given joesPrompt: PreferredPrompt =
    PreferredPrompt("relax> ")

import JillsPrefs.jillsPrompt
import JoesPrefs.joesPrompt
val x = Greeter.greet("Who's there?") // error
