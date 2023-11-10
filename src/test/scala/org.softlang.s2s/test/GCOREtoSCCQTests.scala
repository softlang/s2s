package org.softlang.s2s.test

import de.pseifer.shar.Shar

import org.softlang.s2s.core.Var
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.AtomicPattern

class GCOREtoSCCQTests extends munit.FunSuite:
  import GCORE._

  val shar = Shar()
  import shar._

  // Algorithm:
  //  - Take all When
  //  -   Build Where
  //  - Take all When, union Set, diff Remove
  //  -   Build Construct

  val gq1 = GCORE(
      Construct(
        Set(BasicGraphPattern.NodePattern(Variable("x"))),
        set = Set(SetClause.SetLabel(Variable("x"), Label("Dog"))),
        remove = Set(RemoveClause.RemoveLabel(Variable("x"), Label("Person")))
      ),
      Match(
        Set(BasicGraphPattern.NodePattern(Variable("x"))),
        when = Set(
          WhenClause.HasLabel(Variable("x"), Label("Person")),
          WhenClause.HasLabel(Variable("x"), Label("Friendly"))
        )
      )
    )

  val sq1 = SCCQ(
    template = List(
      AtomicPattern.VAC(Var("x"), Label("Friendly").toIri),
      AtomicPattern.VAC(Var("x"), Label("Dog").toIri)
    ), 
    pattern = List(
      AtomicPattern.VAC(Var("x"), Label("Person").toIri),
      AtomicPattern.VAC(Var("x"), Label("Friendly").toIri)
    )
  )

  // Convert GCORE 'g' to SCCQ, test vs 's'.
  def work(g: GCORE, s: SCCQ, debug: Boolean = false): Unit =
    if debug then
      println("-- given sccq")
      println(sq1.show)
      println("\n-- given gcore")
      println(gq1.show)
      println("\n-- converted gcore -> sccq")
      println(gq1.toSCCQ.map(_.show))
    val conv = g.toSCCQ
    assert(conv.isDefined)
    assert(SCCQ.validate(conv.get, "invalidForThisTest&*@!!!!").isRight)
    assertEquals(conv.get.template, sq1.template)
    assertEquals(conv.get.pattern, sq1.pattern)

  // Test that given query can not be converted 
  // to a valid SCCQ.
  def workInvalid(g: GCORE): Unit =
    val conv = g.toSCCQ
    assert(false)
    // TODO

  test("Minimal G-CORE query to SCCQ query") {
    work(gq1, sq1)
  }
