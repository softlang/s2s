package org.softlang.s2s.test

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._

import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.AtomicPattern

class GCOREtoSCCQTests extends munit.FunSuite:
  import GCORE._

  val shar = Shar()
  import shar._

  implicit val scopes: Scopes = Scopes("•", in = 0, med = 1, out = 2, variable = -1)

  // GCORE 'g' converts successfully to SCCQ, test vs 's'.
  def assertConvertsTo(g: GCORE, s: SCCQ, debug: Boolean = false): Unit =
    if debug then
      println("-- given sccq")
      println(s.show)
      println("\n-- given gcore")
      println(g.show)
      println("\n-- converted gcore -> sccq")
      println(g.toSCCQ(Set()).map(_.show))
    val conv = g.toSCCQ(Set())
    assert(conv.isDefined)
    assert(SCCQ.validate(conv.get, "invalidForThisTest&*@!!!!").isRight)
    assertEquals(conv.get.template.toSet, s.template.toSet)
    assertEquals(conv.get.pattern.toSet, s.pattern.toSet)

  // Given query can not be converted to a valid SCCQ.
  def assertInvalid(g: GCORE): Unit =
    val conv = g.toSCCQ(Set())
    assert(conv.isEmpty)

  // Basic conversion between Iri/Var and Label, Key, Variable, and Values.
  
  test("label conversion works correctly") {
    val l1 = Label("test")
    assertEquals(Label.fromIri(l1.toIri), l1)
    val l2 = Label("AnotherTest")
    assertEquals(Label.fromIri(l2.toIri), l2)
    val l3 = Label("")
    assertEquals(Label.fromIri(l3.toIri), l3)
  }

  test("key conversion works correctly") {
    val k1 = Key("test")
    assertEquals(Key.fromIri(k1.toIri), k1)
    val k2 = Key("AnotherTest")
    assertEquals(Key.fromIri(k2.toIri), k2)
    val k3 = Key("")
    assertEquals(Key.fromIri(k3.toIri), k3)
  }

  test("value conversion works correctly") {
    val v1 = Value.IntValue(0)
    assertEquals(Value.fromIri(v1.toIri), v1)
    val v2 = Value.IntValue(42)
    assertEquals(Value.fromIri(v2.toIri), v2)
    val v3 = Value.BooleanValue(true)
    assertEquals(Value.fromIri(v3.toIri), v3)
    val v4 = Value.BooleanValue(false)
    assertEquals(Value.fromIri(v4.toIri), v4)
    val v5 = Value.StringValue("")
    assertEquals(Value.fromIri(v5.toIri), v5)
    val v6 = Value.StringValue("test")
    assertEquals(Value.fromIri(v6.toIri), v6)
  }

  test("variable conversion works correctly") {
    val v1 = Variable("x")
    assertEquals(Variable.fromIri(v1.toIri), v1)
    assertEquals(Variable.fromVar(v1.toVar), v1)
    val v2 = Variable("abc")
    assertEquals(Variable.fromIri(v2.toIri), v2)
    assertEquals(Variable.fromVar(v2.toVar), v2)
  }

  // Tests for conversion of Shapes to SetClause.
  
  test("shape to set clause") {
    val v1 = Variable("x")
    val l1 = Label("Person")
    assertEquals(GCORE.shapeToSetClause(
      SHACLShape(Subsumption(NamedConcept(v1.toIri), NamedConcept(l1.toIri)))
    ), Right(SetClause.SetLabel(v1, l1)))

    val k1 = Key("hasName")
    val a1 = Value.StringValue("Tim")
    assertEquals(GCORE.shapeToSetClause(
      SHACLShape(Subsumption(NamedConcept(v1.toIri), Existential(NamedRole(k1.toIri), NominalConcept(a1.toIri))))
    ), Right(SetClause.SetKeyValue(v1, k1, a1)))
  }

  // Tests dealing node labels.

  test("pure node fails") {
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

  test("single when succeeds") {
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

  test("when = remove fails") {
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

  test("when <:< remove fails") {
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

  test("when <:< remove succeeds, if set") {
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

  test("labels succeeds") {
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

  // Tests dealing edge labels.
  
  val out = GCORE.nodeToEdgeIri
  val in = GCORE.edgeToNodeIri

  test("pure edge fails") {
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

  test("pure edge fails, even non-empty nodes") {
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

  test("single edge label (when)") {
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
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y"))
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y"))
        )
      )
    )
  }

  test("multiple edge label (when)") {
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
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("likes").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y"))
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("likes").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y"))
        )
      )
    )
  }

  test("multiple edge labels") {
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
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("hates").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y"))
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("likes").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y"))
        )
      )
    )
  }

  // Tests dealing node and edge labels.
  
  test("both labels") {
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
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("hates").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VAC(Var("x"), Label("Person").toIri)
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("likes").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VAC(Var("x"), Label("Person").toIri)
        )
      )
    )
  }

  test("other labels") {
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
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("hates").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri),
        ), 
        pattern = List(
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("likes").toIri),
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri)
        )
      )
    )
  }

  test("many labels") {
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
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("hates").toIri),
          AtomicPattern.VAC(Var("x"), Label("Person").toIri),
          AtomicPattern.VAC(Var("x"), Label("QuasiCat").toIri),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri),
          AtomicPattern.VAC(Var("y"), Label("Animal").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VAC(Var("e"), Label("knows").toIri),
          AtomicPattern.VAC(Var("e"), Label("likes").toIri),
          AtomicPattern.VAC(Var("x"), Label("Person").toIri),
          AtomicPattern.VAC(Var("x"), Label("DogLover").toIri),
          AtomicPattern.VAC(Var("y"), Label("Dog").toIri),
          AtomicPattern.VAC(Var("y"), Label("Animal").toIri),
        )
      )
    )
  }

  // Tests dealing node key-value pairs.

  test("single key-value succeeds") {
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
            WhenClause.HasKeyValue(Variable("x"), Key("name"), Value.StringValue("Tim"))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
        )
      )
    )
  }

  test("multiple key-value succeeds") {
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
            WhenClause.HasKeyValue(Variable("x"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("x"), Key("age"), Value.IntValue(42)),
            WhenClause.HasKeyValue(Variable("x"), Key("employed"), Value.BooleanValue(true))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("x"), Key("employed").toIri, Value.BooleanValue(true).toIri),
        ), 
        pattern = List(
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("x"), Key("employed").toIri, Value.BooleanValue(true).toIri),
        )
      )
    )
  }

  test("set/remove for key-value succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(
            SetClause.SetKeyValue(Variable("x"), Key("age"), Value.IntValue(42))
          ),
          remove = Set(
            RemoveClause.RemoveKey(Variable("x"), Key("employed"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasKeyValue(Variable("x"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("x"), Key("employed"), Value.BooleanValue(true))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri)
        ), 
        pattern = List(
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("employed").toIri, Value.BooleanValue(true).toIri),
        )
      )
    )
  }

  test("set for same key succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(
            SetClause.SetKeyValue(Variable("x"), Key("age"), Value.IntValue(43))
          ),
          remove = Set()
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasKeyValue(Variable("x"), Key("age"), Value.IntValue(42))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(43).toIri),
        ), 
        pattern = List(
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
        )
      )
    )
  }

  test("many key-values succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          set = Set(
            SetClause.SetKeyValue(Variable("x"), Key("age"), Value.IntValue(43))
          ),
          remove = Set(
            RemoveClause.RemoveKey(Variable("x"), Key("employed"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.NodePattern(Variable("x"))),
          when = Set(
            WhenClause.HasKeyValue(Variable("x"), Key("age"), Value.IntValue(42)),
            WhenClause.HasKeyValue(Variable("x"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("x"), Key("employed"), Value.BooleanValue(true))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(43).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("employed").toIri, Value.BooleanValue(true).toIri),
        )
      )
    )
  }

  // Tests dealing edge key-value pairs.

  test("many key-values for edges succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetKeyValue(Variable("e"), Key("since"), Value.IntValue(2001))
          ),
          remove = Set(
            RemoveClause.RemoveKey(Variable("e"), Key("manager"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasKeyValue(Variable("e"), Key("since"), Value.IntValue(2000)),
            WhenClause.HasKeyValue(Variable("e"), Key("role"), Value.StringValue("HR")),
            WhenClause.HasKeyValue(Variable("e"), Key("manager"), Value.BooleanValue(false))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VPL(Var("e"), Key("since").toIri, Value.IntValue(2001).toIri),
          AtomicPattern.VPL(Var("e"), Key("role").toIri, Value.StringValue("HR").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VPL(Var("e"), Key("since").toIri, Value.IntValue(2000).toIri),
          AtomicPattern.VPL(Var("e"), Key("role").toIri, Value.StringValue("HR").toIri),
          AtomicPattern.VPL(Var("e"), Key("manager").toIri, Value.BooleanValue(false).toIri),
        )
      )
    )
  }

  // Tests dealing edge and node key-value pairs.

  test("many key-values for edges succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetKeyValue(Variable("e"), Key("since"), Value.IntValue(2001)),
            SetClause.SetKeyValue(Variable("x"), Key("age"), Value.IntValue(43)),
            SetClause.SetKeyValue(Variable("y"), Key("age"), Value.IntValue(43))
          ),
          remove = Set(
            RemoveClause.RemoveKey(Variable("e"), Key("manager")),
            RemoveClause.RemoveKey(Variable("x"), Key("employed")),
            RemoveClause.RemoveKey(Variable("y"), Key("employed"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasKeyValue(Variable("e"), Key("since"), Value.IntValue(2000)),
            WhenClause.HasKeyValue(Variable("e"), Key("role"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("e"), Key("manager"), Value.BooleanValue(false)),
            WhenClause.HasKeyValue(Variable("x"), Key("age"), Value.IntValue(42)),
            WhenClause.HasKeyValue(Variable("x"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("x"), Key("employed"), Value.BooleanValue(true)),
            WhenClause.HasKeyValue(Variable("y"), Key("age"), Value.IntValue(42)),
            WhenClause.HasKeyValue(Variable("y"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("y"), Key("employed"), Value.BooleanValue(true))
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VPL(Var("e"), Key("since").toIri, Value.IntValue(2001).toIri),
          AtomicPattern.VPL(Var("e"), Key("role").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(43).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("y"), Key("age").toIri, Value.IntValue(43).toIri),
          AtomicPattern.VPL(Var("y"), Key("name").toIri, Value.StringValue("Tim").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VPL(Var("e"), Key("since").toIri, Value.IntValue(2000).toIri),
          AtomicPattern.VPL(Var("e"), Key("role").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("e"), Key("manager").toIri, Value.BooleanValue(false).toIri),
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("employed").toIri, Value.BooleanValue(true).toIri),
          AtomicPattern.VPL(Var("y"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("y"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("y"), Key("employed").toIri, Value.BooleanValue(true).toIri),
        )
      )
    )
  }

  // Tests dealing edge and node key-value pairs and labels.

  test("many key-values for edges succeeds") {
    assertConvertsTo(
      GCORE(
        template = Construct(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          set = Set(
            SetClause.SetKeyValue(Variable("e"), Key("since"), Value.IntValue(2001)),
            SetClause.SetKeyValue(Variable("x"), Key("age"), Value.IntValue(43)),
            SetClause.SetKeyValue(Variable("y"), Key("age"), Value.IntValue(43)),
            SetClause.SetLabel(Variable("x"), Label("A2"))
          ),
          remove = Set(
            RemoveClause.RemoveKey(Variable("e"), Key("manager")),
            RemoveClause.RemoveKey(Variable("x"), Key("employed")),
            RemoveClause.RemoveKey(Variable("y"), Key("employed")),
            RemoveClause.RemoveLabel(Variable("x"), Label("A1"))
          )
        ),
        pattern = Match(
          Set(BasicGraphPattern.EdgePattern(Variable("x"), Variable("e"), Variable("y"))),
          when = Set(
            WhenClause.HasKeyValue(Variable("e"), Key("since"), Value.IntValue(2000)),
            WhenClause.HasKeyValue(Variable("e"), Key("role"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("e"), Key("manager"), Value.BooleanValue(false)),
            WhenClause.HasKeyValue(Variable("x"), Key("age"), Value.IntValue(42)),
            WhenClause.HasKeyValue(Variable("x"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("x"), Key("employed"), Value.BooleanValue(true)),
            WhenClause.HasKeyValue(Variable("y"), Key("age"), Value.IntValue(42)),
            WhenClause.HasKeyValue(Variable("y"), Key("name"), Value.StringValue("Tim")),
            WhenClause.HasKeyValue(Variable("y"), Key("employed"), Value.BooleanValue(true)),
            WhenClause.HasLabel(Variable("x"), Label("A")),
            WhenClause.HasLabel(Variable("x"), Label("A1")),
            WhenClause.HasLabel(Variable("y"), Label("B")),
            WhenClause.HasLabel(Variable("e"), Label("c")),
          )
        )
      ),
      SCCQ(
        template = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VPL(Var("e"), Key("since").toIri, Value.IntValue(2001).toIri),
          AtomicPattern.VPL(Var("e"), Key("role").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(43).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("y"), Key("age").toIri, Value.IntValue(43).toIri),
          AtomicPattern.VPL(Var("y"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VAC(Var("e"), Label("c").toIri),
          AtomicPattern.VAC(Var("x"), Label("A").toIri),
          AtomicPattern.VAC(Var("x"), Label("A2").toIri),
          AtomicPattern.VAC(Var("y"), Label("B").toIri),
        ), 
        pattern = List(
          AtomicPattern.VPV(Var("x"), out, Var("e")),
          AtomicPattern.VPV(Var("e"), in, Var("y")),
          AtomicPattern.VPL(Var("e"), Key("since").toIri, Value.IntValue(2000).toIri),
          AtomicPattern.VPL(Var("e"), Key("role").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("e"), Key("manager").toIri, Value.BooleanValue(false).toIri),
          AtomicPattern.VPL(Var("x"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("x"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("x"), Key("employed").toIri, Value.BooleanValue(true).toIri),
          AtomicPattern.VPL(Var("y"), Key("age").toIri, Value.IntValue(42).toIri),
          AtomicPattern.VPL(Var("y"), Key("name").toIri, Value.StringValue("Tim").toIri),
          AtomicPattern.VPL(Var("y"), Key("employed").toIri, Value.BooleanValue(true).toIri),
          AtomicPattern.VAC(Var("e"), Label("c").toIri),
          AtomicPattern.VAC(Var("x"), Label("A").toIri),
          AtomicPattern.VAC(Var("x"), Label("A1").toIri),
          AtomicPattern.VAC(Var("y"), Label("B").toIri),
        )
      )
    )
  }

