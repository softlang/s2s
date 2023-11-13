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

  // GCORE 'g' converts successfully to SCCQ, test vs 's'.
  def assertConvertsTo(g: GCORE, s: SCCQ, debug: Boolean = false): Unit =
    if debug then
      println("-- given sccq")
      println(s.show)
      println("\n-- given gcore")
      println(g.show)
      println("\n-- converted gcore -> sccq")
      println(g.toSCCQ.map(_.show))
    val conv = g.toSCCQ
    assert(conv.isDefined)
    assert(SCCQ.validate(conv.get, "invalidForThisTest&*@!!!!").isRight)
    assertEquals(conv.get.template, s.template)
    assertEquals(conv.get.pattern, s.pattern)

  // Given query can not be converted to a valid SCCQ.
  def assertInvalid(g: GCORE): Unit =
    val conv = g.toSCCQ
    assert(conv.isEmpty)

  // Tests dealing with labels.

  test("Converting GCORE with pure node fails") {
    assertInvalid(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set()
        )
      )
    )
  }

  test("Converting GCORE with single when succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasLabel(Variable("x"), Label("Person"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VAC(Var("x"), Label("Person").toIri)
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("x"), Label("Person").toIri),
        )
      )
    )
  }

  test("Converting GCORE with when = remove fails") {
    assertInvalid(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("x"), Label("Person"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasLabel(Variable("x"), Label("Person"))
          )
        )
      )
    )
  }

  test("Converting GCORE with when <:< remove fails") {
    assertInvalid(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("x"), Label("Person")),
            RemoveClause.RemoveLabel(Variable("x"), Label("Dog"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasLabel(Variable("x"), Label("Person"))
          )
        )
      )
    )
  }

  test("Converting GCORE with when <:< remove succeeds, if set") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(
            SetClause.SetLabel(Variable("x"), Label("Dog"))
          ),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("x"), Label("Person")),
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasLabel(Variable("x"), Label("Person"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VAC(Var("x"), Label("Dog").toIri)
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("x"), Label("Person").toIri)
        )
      )
    )
  }

  test("Converting (valid) GCORE with labels succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(SetClause.SetLabel(Variable("x"), Label("Dog"))),
          remove = Set(RemoveClause.RemoveLabel(Variable("x"), Label("Person")))
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasLabel(Variable("x"), Label("Person")),
            WhenClause.HasLabel(Variable("x"), Label("Friendly"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VAC(Var("x"), Label("Friendly").toIri),
          AtomicPattern.VAC(Var("x"), Label("Dog").toIri)
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("x"), Label("Person").toIri),
          AtomicPattern.VAC(Var("x"), Label("Friendly").toIri)
        )
      )
    )
  }

