//> using options -Xfatal-warnings
@main def main() = Macro.makeMatch()
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
