package org.softlang.s2s.test

import de.pseifer.shar.dl._
import org.softlang.s2s.core._
import org.softlang.s2s.query._
import org.softlang.s2s.query.inScope
import org.softlang.s2s.infer.SubsumptionsFromMappings
import org.softlang.s2s.infer.MappingSubsumption

class MappingSubsumptionTests extends munit.FunSuite with TestData:

  def workOld(a: AtomicPatterns, shapes: Set[SimpleSHACLShape]): Set[Axiom] =
    SubsumptionsFromMappings(a, shapes, debug = false).axioms

  def workNew(a: AtomicPatterns, axioms: Set[Axiom]): Set[Axiom] =
    MappingSubsumption(a, axioms, debug = true).axioms

  def work(a: AtomicPatterns, shapes: Set[SimpleSHACLShape]): Set[Axiom] =
    val preA = a.inScope(Scope.Med)
    val o = workOld(preA, shapes)
    //val n = workNew(preA, shapes.map(_.axiom)) // not sufficient, possibly
    //assertEquals(o, n)
    o

  test("no subsumption 1") {
    assertEquals(work(q1.pattern, Set()), Set())
    assertEquals(work(q1.pattern, Sall), Set())
  }

  test("no subsumption 2") {
    assertEquals(work(q2.pattern, Set()), Set())
    assertEquals(work(q2.pattern, Sall), Set())
  }

  test("simple subsumption") {
    assertEquals(work(q3.pattern, Set()), Set(
      Subsumption(yq.asConcept, xq.asConcept),
      Subsumption(xq.asConcept, yq.asConcept),
    ))
  }

  test("simple extended subsumption (fail)") {
    assertEquals(work(q4.pattern, Set()), Set())
  }

  test("simple extended subsumption (ok)") {
    assertEquals(work(q4.pattern, Set(s1)), Set(
      Subsumption(xq.asConcept, yq.asConcept)
    ))
  }

  test("no subsumption 3") {
    assertEquals(work(q5.pattern, Set()), Set())
    assertEquals(work(q5.pattern, Sall), Set())
  }

  test("no subsumption 4") {
    assertEquals(work(q6.pattern, Set()), Set())
    assertEquals(work(q6.pattern, Sall), Set())
  }

  test("no subsumption 5") {
    assertEquals(work(q7.pattern, Set()), Set())
    assertEquals(work(q7.pattern, Sall), Set())
  }

  test("no subsumption 6") {
    assertEquals(work(q8.pattern, Set()), Set())
    assertEquals(work(q8.pattern, Set(s1,s3,s5)), Set())
  }

  test("subsumption 1") {
    assertEquals(work(q8.pattern, Set(s4)), Set(
      Subsumption(xq.asConcept, yq.asConcept),
    ))
  }

  test("no subsumption 7") {
    assertEquals(work(q9.pattern, Set()), Set())
  }

  test("subsumption 2") {
    assertEquals(work(q9.pattern, Set(s3)), Set(
      Subsumption(yq.asConcept, xq.asConcept),
    ))
  }

  val q13p = 
    List(
      AtomicPattern.VAC(x, D),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VAC(z, C),
    )

  test("no subsumption 8") {
    assertEquals(work(q13p, Set()), Set())
    assertEquals(work(q13p, Set(s4)), Set())
  }

  test("subsumption 3") {
    assertEquals(work(q13p, Set(s5)), Set(
      Subsumption(xq.asConcept, yq.asConcept),
    ))
  }

  test("subsumption 4") {
    assertEquals(work(q13p, Set(s1)), Set(
      Subsumption(zq.asConcept, xq.asConcept),
    ))
  }

  test("subsumption 5") {
    assertEquals(work(q13p, Set(s3)), Set(
      Subsumption(zq.asConcept, yq.asConcept),
    ))
  }

  test("subsumption 6") {
    assertEquals(work(q13p, Set(s1,s3,s5)), Set(
      Subsumption(zq.asConcept, xq.asConcept),
      Subsumption(zq.asConcept, yq.asConcept),
      Subsumption(xq.asConcept, yq.asConcept),
    ))
  }

