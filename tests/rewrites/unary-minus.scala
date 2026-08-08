//rewrite
import util.chaining.*

def f = - 42
def g = -42.abs
def h = - 42.abs
def i = - 42
  .pipe(_.abs)

def fL = - 42L
def gL = -42L.abs
def hL = - 42L.abs
def iL = - 42L
  .pipe(_.abs)

def fF = - 42f
def gF = -42f.abs
def hF = - 42f.abs
def iF = - 42f
  .pipe(_.abs)

def fD = - 42d
def gD = -42d.abs
def hD = - 42d.abs
def iD = - 42d
  .pipe(_.abs)