import scala.reflect.{classTag, ClassTag, NoManifest}

@main def Test: Unit =

  /* ====== no manifest available ====== */

  locally {
    noManifest[Array[? <: Int]] // available as a manifest
    noManifest[Array[? <: String]] // available as a manifest
    noManifest[Array[Nothing]]
    noManifest[Array[Null]]
    noManifest[Nothing]
    noManifest[Null]
  }

  /* ====== ClassTag and OptManifest have the same runtime class and same equality ======= */

  locally {
    interopOpt[List[Int]]
    interopOpt[List[? <: Int]]
  }

  /* ====== Test some OptManifest have the same runtime class and are equal ======= */

  locally {
    sameClassEqualOpt[List[Int], List[? <: Int]] // not equal for full manifests
    sameClassEqualOpt[List[Int], List[String]]   // not equal for full manifests
  }

  /* ============================================================================= */
  // The following tests rely on <:< being correct, i.e. `equals` on Manifest      //
  // uses `<:<` underneath.                                                        //
  /* ============================================================================= */

  /* ====== Test some Manifest have the same runtime class and are equal ======= */

  locally {
    trait A
    trait B {def b: Int}
    trait C {def c: Int}
    trait D {def d: Int}
    class fooAnnot extends scala.annotation.StaticAnnotation

    type SomeRefinedType =
      ((B {def b: 0} & C) & ((A @fooAnnot) & D {def d: 2})) {def c: 1}

    sameClassEqualMan[Array[? <: String], Array[String]]
    sameClassEqualMan[SomeRefinedType, A]
  }


  /* ====== Test some Manifest have the same runtime class but are not equal ======= */

  locally {
    sameClassNonEqualMan[List[Int], List[? <: Int]]
    sameClassNonEqualMan[List[Int], List[String]]
  }

  /* ====== Test that some Manifest have the same runtime class, are not equal, but are `<:<` ======= */

  locally {
    class A
    class B extends A

    sameClassSub[List[Int], List[AnyVal]]
    sameClassSub[List[Unit], List[AnyVal]]
    sameClassSub[List[B], List[A]]
    sameClassSub[Array[List[B]], Array[List[A]]]
  }

end Test

def noManifest[A: OptManifest] =
  assert(optManifest[A] eq NoManifest)

def interopOpt[A: ClassTag: OptManifest] =
  assert(classTag[A] == optManifest[A])
  optManifest[A] match
    case optA: ClassTag[_] =>
      assert(classTag[A].runtimeClass == optA.runtimeClass)

def sameClassEqualOpt[A: OptManifest, B: OptManifest] =
  assert(optManifest[A] == optManifest[B])
  (optManifest[A], optManifest[B]) match
    case (a: ClassTag[_], b: ClassTag[_]) =>
      assert(a.runtimeClass == b.runtimeClass)

def sameClassMan[A: Manifest, B: Manifest] =
  assert(manifest[A].runtimeClass == manifest[B].runtimeClass)

def sameClassEqualMan[A: Manifest, B: Manifest] =
  sameClassMan[A, B]
  assert(manifest[A] == manifest[B])

def sameClassNonEqualMan[A: Manifest, B: Manifest] =
  sameClassMan[A, B]
  assert(manifest[A] != manifest[B])

def sameClassSub[A: Manifest, B: Manifest] =
  sameClassNonEqualMan[A, B]
  assert(manifest[A] <:< manifest[B])
