package experiments.issues.suspendparams

import language.experimental.captureChecking

trait State[S, R] {
  def get(resume: S => R): R
  def put(newState: S, resume: Unit => R): R^{resume}
}

type R = (Int, Long)
def run[S1 <: State[?, R], S2 <: State[?, R]](s1: S1, s2: S2)(prog: (S1, S2) => R): R = prog(s1, s2)

inline def program(sum: State[Long, R]): R = {
  def rec(x: Int): R = {
    sum.get { s0 =>
      val s = s0 + x
      sum.put(s, _ => {
        if x > 0 then rec(x - 1) else (x, s)
      })
    }
  }
  rec(10)
}

def runProgram(other: State[?, R], sum: State[Long, R]) = {
  run(other, sum)((_, s) => program(s))
}