package scala.quoted.staging

class RunScopeException extends Exception("Cannot call `scala.quoted.staging.run(...)` within a macro or another `run(...)`")
