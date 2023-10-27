package org.softlang.s2s.test

import de.pseifer.shar.core.Iri
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.query.AtomicPattern
import org.softlang.s2s.query.SCCQ

trait TestData:

  implicit val scopes: Scopes = Scopes("-")

  // Concepts

  val A = Iri.fromString("<https://github.com/softlang/s2s/A>").toOption.get
  val B = Iri.fromString("<https://github.com/softlang/s2s/B>").toOption.get
  val C = Iri.fromString("<https://github.com/softlang/s2s/C>").toOption.get
  val D = Iri.fromString("<https://github.com/softlang/s2s/D>").toOption.get

  val Am = Iri.fromString("<https://github.com/softlang/s2s/A->").toOption.get
  val Bm = Iri.fromString("<https://github.com/softlang/s2s/B->").toOption.get
  val Cm = Iri.fromString("<https://github.com/softlang/s2s/C->").toOption.get
  val Dm = Iri.fromString("<https://github.com/softlang/s2s/D->").toOption.get

  val Ao = Iri.fromString("<https://github.com/softlang/s2s/A-->").toOption.get
  val Bo = Iri.fromString("<https://github.com/softlang/s2s/B-->").toOption.get
  val Co = Iri.fromString("<https://github.com/softlang/s2s/C-->").toOption.get
  val Do = Iri.fromString("<https://github.com/softlang/s2s/D-->").toOption.get

  // Properties
  
  val p = Iri.fromString("<https://github.com/softlang/s2s/p>").toOption.get
  val r = Iri.fromString("<https://github.com/softlang/s2s/r>").toOption.get

  val pm = Iri.fromString("<https://github.com/softlang/s2s/p->").toOption.get
  val rm = Iri.fromString("<https://github.com/softlang/s2s/r->").toOption.get

  val po = Iri.fromString("<https://github.com/softlang/s2s/p-->").toOption.get
  val ro = Iri.fromString("<https://github.com/softlang/s2s/r-->").toOption.get

  // Variables

  val w = Var("w")
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

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
