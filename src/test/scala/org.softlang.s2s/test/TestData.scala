package org.softlang.s2s.test

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._

import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.query.AtomicPattern
import org.softlang.s2s.query.SCCQ

/** Test example data for internal unit tests. */
trait TestData:

  implicit val scopes: Scopes = Scopes("•", 0, 1, 2, -1)

  // IRIs (concept names)

  val A = Iri.fromString("<https://github.com/softlang/s2s/A•0>").toOption.get
  val B = Iri.fromString("<https://github.com/softlang/s2s/B•0>").toOption.get
  val C = Iri.fromString("<https://github.com/softlang/s2s/C•0>").toOption.get
  val D = Iri.fromString("<https://github.com/softlang/s2s/D•0>").toOption.get

  val Am = Iri.fromString("<https://github.com/softlang/s2s/A•1>").toOption.get
  val Bm = Iri.fromString("<https://github.com/softlang/s2s/B•1>").toOption.get
  val Cm = Iri.fromString("<https://github.com/softlang/s2s/C•1>").toOption.get
  val Dm = Iri.fromString("<https://github.com/softlang/s2s/D•1>").toOption.get

  val Ao = Iri.fromString("<https://github.com/softlang/s2s/A•2>").toOption.get
  val Bo = Iri.fromString("<https://github.com/softlang/s2s/B•2>").toOption.get
  val Co = Iri.fromString("<https://github.com/softlang/s2s/C•2>").toOption.get
  val Do = Iri.fromString("<https://github.com/softlang/s2s/D•2>").toOption.get

  // IRIs (role names)
  
  val p = Iri.fromString("<https://github.com/softlang/s2s/p•0>").toOption.get
  val r = Iri.fromString("<https://github.com/softlang/s2s/r•0>").toOption.get

  val pm = Iri.fromString("<https://github.com/softlang/s2s/p•1>").toOption.get
  val rm = Iri.fromString("<https://github.com/softlang/s2s/r•1>").toOption.get

  val po = Iri.fromString("<https://github.com/softlang/s2s/p•2>").toOption.get
  val ro = Iri.fromString("<https://github.com/softlang/s2s/r•2>").toOption.get

  // Variables

  val w = Var("w")
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

  val wq = Var("w•-1")
  val xq = Var("x•-1")
  val yq = Var("y•-1")
  val zq = Var("z•-1")

  val x1 = Var("x1")
  val y1 = Var("y1")
  val x2 = Var("x2")
  val y2 = Var("y2")
  val x3 = Var("x3")
  val y3 = Var("y3")
  val x4 = Var("x4")
  val y4 = Var("y4")
  val x5 = Var("x5")
  val y5 = Var("y5")

  val x1q = Var("x1•-1")
  val y1q = Var("y1•-1")
  val x2q = Var("x2•-1")
  val y2q = Var("y2•-1")
  val x3q = Var("x3•-1")
  val y3q = Var("y3•-1")
  val x4q = Var("x4•-1")
  val y4q = Var("y4•-1")
  val x5q = Var("x5•-1")
  val y5q = Var("y5•-1")

  // (Simple)SHACLShapes
  
  val s1 = SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(D)))
  val s2 = SHACLShape(Subsumption(NamedConcept(C), Union(NamedConcept(A), NamedConcept(B))))
  val s3 = SimpleSHACLShape(Subsumption(NamedConcept(C), Existential(NamedRole(p), NamedConcept(C))))
  val s4 = SimpleSHACLShape(Subsumption(NamedConcept(C), Existential(Inverse(NamedRole(p)), NamedConcept(C))))
  val s5 = SimpleSHACLShape(Subsumption(NamedConcept(D), Existential(NamedRole(p), NamedConcept(C))))

  val Sall = Set(s1, s3, s4, s5)

  // Queries (empty)

  val q0 = SCCQ(Nil, Nil)

  // Queries (concepts only)

  val q1 = SCCQ(
    List(AtomicPattern.VAC(x, C)), 
    List(AtomicPattern.VAC(x, C)))

  val q2 = SCCQ(
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VAC(x, D)
    ), 
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VAC(x, D)
    ))

  val q3 = SCCQ(
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VAC(y, C)
    ), 
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VAC(y, C)
    ))

  val q4 = SCCQ(
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VAC(y, D)
    ), 
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VAC(y, D)
    ))

  // Queries (properties only)

  val q5 = SCCQ(
    List(
      AtomicPattern.VPV(x, p, y),
    ), 
    List(
      AtomicPattern.VPV(x, p, y),
    ))

  val q6 = SCCQ(
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(x, r, y)
    ), 
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(x, r, y)
    ))

  val q7 = SCCQ(
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, r, z)
    ), 
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, r, z)
    ))

  // Queries (general)
  
  val q8 = SCCQ(
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(x, C),
    ), 
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(x, C),
    ))

  val q9 = SCCQ(
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(y, C),
    ), 
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(y, C),
    ))

  val q10 = SCCQ(
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(y, C),
    ), 
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(y, C),
    ))

  val q11 = SCCQ(
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(y, D),
    ), 
    List(
      AtomicPattern.VAC(x, C),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VAC(y, D),
    ))

  // Queries (cyclic dependency graph)

  val q12 = SCCQ(
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VPV(z, p, x),
    ), 
    List(
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VPV(z, p, x),
    ))

