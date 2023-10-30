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

  // (Simple)SHACLShapes
  
  val s1 = SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(D)))
  val s2 = SHACLShape(Subsumption(NamedConcept(C), Union(NamedConcept(A), NamedConcept(B))))

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

