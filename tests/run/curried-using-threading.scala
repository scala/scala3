case class Context(str: String)

def root(using Context): String = summon[Context].str

def narrow(using Context)(owner: String = root) = owner

given Context("alpha")

@main def Test = assert(narrow() == "alpha")

