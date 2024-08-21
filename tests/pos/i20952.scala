package object packer: // the super class needs to be in a different package
  class SuperClass():
    protected val problem: Any = ??? // needs to be protected

class SuperClass():
  protected val problem: Any = ??? // needs to be protected

// type Target = SuperClass        // passes
type Target = packer.SuperClass // error

trait Child extends Target:

  val aliased: problem.type = problem
  type Alias = problem.type

  val newProblem: Any {val prog: problem.type} = ???  // error
  val newProblem2: Any {val prog: Alias} = ???         // passes
  val newProblem3: Any {val prog: aliased.type} = ???  // passes

class ChildImpl extends Target with Child // concrete implementation is needed
