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

  // Tests dealing with node labels.

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

  // Tests dealing with edge labels.

  test("Converting GCORE with pure edge fails") {
    assertInvalid(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set()
        )
      )
    )
  }

  test("Converting GCORE with pure edge fails, even with non-empty nodes") {
    assertInvalid(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("x"), Label("Person")),
            WhenClause.HasLabel(Variable("y"), Label("Friendly"))
          )
        )
      )
    )
  }

  test("Converting GCORE with single edge label (when)") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("e"), Label("knows")),
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
        )
      )
    )
  }

  test("Converting GCORE with multiple edge label (when)") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("e"), Label("knows")),
            WhenClause.HasLabel(Variable("e"), Label("likes")),
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("likes").toIri, Var("y")),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("likes").toIri, Var("y")),
        )
      )
    )
  }

  test("Converting GCORE with multiple edge labels") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetLabel(Variable("e"), Label("hates")),
          ),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("e"), Label("likes"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("e"), Label("knows")),
            WhenClause.HasLabel(Variable("e"), Label("likes")),
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("hates").toIri, Var("y")),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("likes").toIri, Var("y")),
        )
      )
    )
  }

  // Tests dealing with node and edge labels.
  
  test("Converting GCORE with both labels") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetLabel(Variable("e"), Label("hates"))
          ),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("e"), Label("likes"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("e"), Label("knows")),
            WhenClause.HasLabel(Variable("e"), Label("likes")),
            WhenClause.HasLabel(Variable("x"), Label("Person"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("hates").toIri, Var("y")),
          AtomicPattern.VAC(Var("x"), Label("Person").toIri)
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("likes").toIri, Var("y")),
          AtomicPattern.VAC(Var("x"), Label("Person").toIri)
        )
      )
    )
  }

  test("Converting GCORE with other labels") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetLabel(Variable("e"), Label("hates"))
          ),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("e"), Label("likes"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("e"), Label("knows")),
            WhenClause.HasLabel(Variable("e"), Label("likes")),
            WhenClause.HasLabel(Variable("y"), Label("Dog"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("hates").toIri, Var("y")),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VPV(Var("x"), Label("likes").toIri, Var("y")),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri)
        )
      )
    )
  }

  test("Converting GCORE with many labels") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetLabel(Variable("e"), Label("hates")),
            SetClause.SetLabel(Variable("x"), Label("QuasiCat")),
          ),
          remove = Set(
            RemoveClause.RemoveLabel(Variable("e"), Label("likes")),
            RemoveClause.RemoveLabel(Variable("x"), Label("DogLover")),
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasLabel(Variable("e"), Label("knows")),
            WhenClause.HasLabel(Variable("e"), Label("likes")),
            WhenClause.HasLabel(Variable("x"), Label("Person")),
            WhenClause.HasLabel(Variable("x"), Label("DogLover")),
            WhenClause.HasLabel(Variable("y"), Label("Dog")),
            WhenClause.HasLabel(Variable("y"), Label("Animal"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VAC(Var("x"), Label("Person").toIri),
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri),
          AtomicPattern.VAC(Var("y"), Label("Animal").toIri),
          AtomicPattern.VAC(Var("x"), Label("QuasiCat").toIri),
          AtomicPattern.VPV(Var("x"), Label("hates").toIri, Var("y"))
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("x"), Label("Person").toIri),
          AtomicPattern.VPV(Var("x"), Label("knows").toIri, Var("y")),
          AtomicPattern.VAC(Var("x"), Label("DogLover").toIri),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri),
          AtomicPattern.VAC(Var("y"), Label("Animal").toIri),
          AtomicPattern.VPV(Var("x"), Label("likes").toIri, Var("y"))
        )
      )
    )
  }
