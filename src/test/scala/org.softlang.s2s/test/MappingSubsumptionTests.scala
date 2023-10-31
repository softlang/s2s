package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import de.pseifer.shar.dl._
import org.softlang.s2s.core._
import org.softlang.s2s.query._
import org.softlang.s2s.query.inScope
import org.softlang.s2s.infer.SubsumptionsFromMappings
import org.softlang.s2s.infer.MappingSubsumption

class MappingSubsumptionTests extends TestData:

  def workOld(a: AtomicPatterns, shapes: Set[SimpleSHACLShape]): Set[Axiom] =
    SubsumptionsFromMappings(a, shapes, debug = false).axioms

  def workNew(a: AtomicPatterns, axioms: Set[Axiom]): Set[Axiom] =
    MappingSubsumption(a, axioms, debug = true).axioms

  def work(a: AtomicPatterns, shapes: Set[SimpleSHACLShape]): Set[Axiom] =
    val preA = a.inScope(Scope.Med)
    val o = workOld(preA, shapes)
    val n = workNew(preA, shapes.map(_.axiom)) // not sufficient, possibly
    assertEquals(o, n)
    o

  /*
  @Test def testNoSubsumption1(): Unit =
    assertEquals(work(q1.pattern, Set()), Set())
    assertEquals(work(q1.pattern, Sall), Set())

  @Test def testNoSubsumption2(): Unit =
    assertEquals(work(q2.pattern, Set()), Set())
    assertEquals(work(q2.pattern, Sall), Set())

  @Test def testSimpleSubsumption(): Unit =
    assertEquals(work(q3.pattern, Set()), Set(
      Subsumption(yq.asConcept, xq.asConcept),
      Subsumption(xq.asConcept, yq.asConcept),
    ))
  */

  @Test def testSimpleExtendedSubsumptionFail(): Unit =
    assertEquals(work(q4.pattern, Set()), Set())

  @Test def testSimpleExtendedSubsumptionOk(): Unit =
    assertEquals(work(q4.pattern, Set(s1)), Set(
      Subsumption(xq.asConcept, yq.asConcept)
    ))

  /*
  @Test def testNoSubsumption3(): Unit =
    assertEquals(work(q5.pattern, Set()), Set())
    assertEquals(work(q5.pattern, Sall), Set())

  @Test def testNoSubsumption4(): Unit =
    assertEquals(work(q6.pattern, Set()), Set())
    assertEquals(work(q6.pattern, Sall), Set())

  @Test def testNoSubsumption5(): Unit =
    assertEquals(work(q7.pattern, Set()), Set())
    assertEquals(work(q7.pattern, Sall), Set())

  @Test def testNoSubsumption6(): Unit =
    assertEquals(work(q8.pattern, Set()), Set())
    assertEquals(work(q8.pattern, Set(s1,s3,s5)), Set())

  @Test def testSubsumption1(): Unit =
    assertEquals(work(q8.pattern, Set(s4)), Set(
      Subsumption(xq.asConcept, yq.asConcept),
    ))

  @Test def testNoSubsumption7(): Unit =
    assertEquals(work(q9.pattern, Set()), Set())

  @Test def testSubsumption2(): Unit =
    assertEquals(work(q9.pattern, Set(s3)), Set(
      Subsumption(yq.asConcept, xq.asConcept),
    ))

  val q13p = 
    List(
      AtomicPattern.VAC(x, D),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VAC(z, C),
    )

  @Test def testNoSubsumption8(): Unit =
    assertEquals(work(q13p, Set()), Set())
    assertEquals(work(q13p, Set(s4)), Set())

  @Test def testSubsumption3(): Unit =
    assertEquals(work(q13p, Set(s5)), Set(
      Subsumption(xq.asConcept, yq.asConcept),
    ))

  @Test def testSubsumption4(): Unit =
    assertEquals(work(q13p, Set(s1)), Set(
      Subsumption(zq.asConcept, xq.asConcept),
    ))

  @Test def testSubsumption5(): Unit =
    assertEquals(work(q13p, Set(s3)), Set(
      Subsumption(zq.asConcept, yq.asConcept),
    ))

  @Test def testSubsumption6(): Unit =
    assertEquals(work(q13p, Set(s1,s3,s5)), Set(
      Subsumption(zq.asConcept, xq.asConcept),
      Subsumption(zq.asConcept, yq.asConcept),
      Subsumption(xq.asConcept, yq.asConcept),
    ))

  */
