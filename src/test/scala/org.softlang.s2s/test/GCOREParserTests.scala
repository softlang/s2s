package org.softlang.s2s.test

import org.softlang.s2s.query.GCORE
import org.softlang.s2s.parser.GCOREParser

class GCOREParserTests extends munit.FunSuite:
  import GCORE._

  // The parser instance to be tested.
  val p = GCOREParser()

  // Test framework.
  
  def parses[T](s: String, v: T)(implicit pp: p.Parser[T]) =
    val res = p.parseAll(pp, s)
    if !res.successful then println(res.toString)
    assert(res.successful)
    assertEquals(res.get, v)

  def parsesButNot[T](s: String, v: T)(implicit pp: p.Parser[T]) =
    val res = p.parseAll(pp, s)
    assert(res.successful)
    assertNotEquals(res.get, v)

  def parsesNot[T](s: String)(implicit pp: p.Parser[T]) =
    val res = p.parseAll(pp, s)
    assert(!(res.successful))

  //def produces(gcore: String, sparql: String) =
  //  val sparqlout = 
  //    GCoreToSparql(":").convert(p(gcore))
  //  assertEquals(sparqlout, sparql)

  // Test cases.

  test("Parsing of Variable") {
    implicit val pp = p.pVariable

    parses("x", Variable("x"))
    parses("X", Variable("X"))
    parses("xXy", Variable("xXy"))
    parses("abc", Variable("abc"))
    parses("x1", Variable("x1"))
    parses("x_1", Variable("x_1"))

    parsesNot("")
    parsesNot("1b")
    parsesNot("_b")
  }

  test("Parsing of Label") {
    implicit val pp = p.pLabel

    parses(":x", Label("x"))
    parses(":X", Label("X"))
    parses(":xXy", Label("xXy"))
    parses(":abc", Label("abc"))
    parses(":x1", Label("x1"))
    parses(":x_1", Label("x_1"))

    parsesNot(":")
    parsesNot(":1b")
    parsesNot(":_b")
  }
  
  test("Parsing of Key") {
    implicit val pp = p.pKey

    parses(".x", Key("x"))
    parses(".X", Key("X"))
    parses(".xXy", Key("xXy"))
    parses(".abc", Key("abc"))
    parses(".x1", Key("x1"))
    parses(".x_1", Key("x_1"))

    parsesNot(".")
    parsesNot(".1b")
    parsesNot("._b")
  }

  test("Parsing of Int Value") {
    implicit val pp = p.pValue

    parses("42", Value.IntValue(42))
    parses("0", Value.IntValue(0))
    parses("-1", Value.IntValue(-1))
  }
    
  test("Parsing of String Value") {
    implicit val pp = p.pValue

    parses("\"abc\"", Value.StringValue("abc"))
    parses("\"123\"", Value.StringValue("123"))
    parses("\"a bc d\"", Value.StringValue("a bc d"))
    parses("\"\"", Value.StringValue(""))
    parses("\" \"", Value.StringValue(" "))
    parses("\"  \"", Value.StringValue("  "))
  }

  test("Parsing of Boolean Value") {
    implicit val pp = p.pValue

    parses("true", Value.BooleanValue(true))
    parses("true", Value.BooleanValue(true))
    parses("false", Value.BooleanValue(false))
  }

  test("Parsing of Value (Fail)") {
    implicit val pp = p.pValue
    parsesButNot("1", Value.IntValue(2))
    parsesButNot("123", Value.StringValue("abc"))
    parsesButNot("0", Value.BooleanValue(true))
    parsesButNot("0", Value.BooleanValue(false))

    parsesNot("truep")
    parsesNot("123p")
    parsesNot("abc")
    parsesNot("")
    parsesNot("--1")
  }

  val nodeBGP = BasicGraphPattern.NodePattern(Variable("x"))
  val edgeBGP = BasicGraphPattern.EdgePattern(Variable("x"), Variable("z"), Variable("y"))

  test("Parsing of BasicGraphPattern") {
    implicit val pp = p.pBasicGraphPattern
    parses("(x)", nodeBGP)
    parses("(longer)", BasicGraphPattern.NodePattern(Variable("longer")))
    parses("(x)-[z]->(y)", edgeBGP)
  }

  val whenLabel = WhenClause.HasLabel(Variable("x"), Label("Person"))
  val whenKey = WhenClause.HasKey(Variable("x"), Key("age"))
  val whenKeyValue = WhenClause.HasKeyValue(Variable("x"), Key("age"), Value.IntValue(42))

  test("Parsing of WhenClause") {
    implicit val pp = p.pWhenClause

    parses("x:Person", whenLabel)
    parses("x :Person", WhenClause.HasLabel(Variable("x"), Label("Person")))
    parses("abc:x", WhenClause.HasLabel(Variable("abc"), Label("x")))

    parses("x.age", whenKey)
    parses("person.x", WhenClause.HasKey(Variable("person"), Key("x")))

    parses("x.age = 42", whenKeyValue)
    parses("x.b=0", WhenClause.HasKeyValue(
      Variable("x"), Key("b"), Value.IntValue(0)))
    parses("person.x = \"test\"", WhenClause.HasKeyValue(
      Variable("person"), Key("x"), Value.StringValue("test")))

    parsesNot("person.")
    parsesNot("person:")
    parsesNot("person =")
    parsesNot("person=")
  }

  test("Parsing of Simple WhenClauses") {
    implicit val pp = p.pWhenClauses

    // Must parse one or more.
    parses("x:Person", Set(whenLabel))
    parses("x:Person AND x.age", Set(whenLabel, whenKey))
    parses("x.age AND x:Person", Set(whenKey, whenLabel))
    parses(" x.age   AND      x:Person ", Set(whenKey, whenLabel))
    parses("x:Person AND x.age AND x.age = 42", Set(whenLabel, whenKey, whenKeyValue))

    // Must be at least one clause (not empty).
    parsesNot("")
  }

  val longMatch = Set(nodeBGP, edgeBGP)

  test("Parsing of Match") {
    implicit val pp = p.pMatch

    parses("MATCH (x)", Match(Set(nodeBGP), Set()))
    parses("MATCH (x)-[z]->(y)", Match(Set(edgeBGP), Set()))

    parses("MATCH (x), (x)-[z]->(y)", Match(longMatch, Set()))
    parses("MATCH (x)-[z]->(y), (x)", Match(longMatch, Set()))
  }

  test("Parsing of Match with WhereClause") {
    implicit val pp = p.pMatch

    parses("MATCH (x) WHERE x:Person", Match(Set(nodeBGP), Set(whenLabel)))
    parses("MATCH (x) WHERE x.age", Match(Set(nodeBGP), Set(whenKey)))
    parses("MATCH (x) WHERE x.age = 42", Match(Set(nodeBGP), Set(whenKeyValue)))

    parses("MATCH  (x)    WHERE    x:Person", Match(Set(nodeBGP), Set(whenLabel)))
    parses("""MATCH (x)
              WHERE x:Person""", Match(Set(nodeBGP), Set(whenLabel)))

    parses("MATCH (x), (x)-[z]->(y) WHERE x:Person", Match(longMatch, Set(whenLabel)))
    parses("MATCH (x)-[z]->(y), (x) WHERE x.age = 42", Match(longMatch, Set(whenKeyValue)))

    parses("MATCH (x), (x)-[z]->(y) WHERE x:Person AND x.age = 42", Match(longMatch, Set(whenLabel, whenKeyValue)))
    parses("MATCH (x)-[z]->(y), (x) WHERE x.age = 42 AND x:Person", Match(longMatch, Set(whenKeyValue, whenLabel)))
  }

  val longConstruct = Set(nodeBGP, edgeBGP)

  test("Parsing of Construct") {
    implicit val pp = p.pConstruct

    parses("CONSTRUCT (x)", Construct(Set(nodeBGP), Set(), Set()))
    parses("CONSTRUCT (x)-[z]->(y)", Construct(Set(edgeBGP), Set(), Set()))

    parses("CONSTRUCT (x), (x)-[z]->(y)", Construct(longConstruct, Set(), Set()))
    parses("CONSTRUCT (x)-[z]->(y), (x)", Construct(longConstruct, Set(), Set()))
  }

  val setLabel = SetClause.SetLabel(Variable("x"), Label("Person"))
  val setKeyValue = SetClause.SetKeyValue(Variable("x"), Key("age"), Value.IntValue(42))

  test("Parsing of SetClauses") {
    implicit val pp = p.pSetClauses

    parses("x:Person", Set(setLabel))
    parses("x.age = 42", Set(setKeyValue))
    parses("x.age = 42 AND x:Person", Set(setKeyValue, setLabel))
    parses("x:Person", Set(setLabel))
    parses("x.age = 42", Set(setKeyValue))
    parses("x.age = 42 AND x:Person", Set(setKeyValue, setLabel))
    parses("x:Person", Set(setLabel))

    parsesNot("")
    parsesNot("x")
    parsesNot("x.age")
  }

  val removeLabel = RemoveClause.RemoveLabel(Variable("x"), Label("Person"))
  val removeKey = RemoveClause.RemoveKey(Variable("x"), Key("age"))

  test("Parsing of RemoveClauses") {
    implicit val pp = p.pRemoveClauses

    parses("x:Person", Set(removeLabel))
    parses("x.age ", Set(removeKey))
    parses("x.age AND x:Person", Set(removeKey, removeLabel))
    parses("x:Person", Set(removeLabel))
    parses("x.age ", Set(removeKey))
    parses("x.age AND x:Person", Set(removeKey, removeLabel))
    parses("x:Person", Set(removeLabel))

    parsesNot("")
    parsesNot("x")
    parsesNot("x.age = 42")
  }

  test("Parsing of Construct with SetClause") {
    implicit val pp = p.pConstruct

    parses("CONSTRUCT (x) SET x:Person", Construct(Set(nodeBGP), Set(setLabel), Set()))
    parses("CONSTRUCT (x) SET x.age = 42", Construct(Set(nodeBGP), Set(setKeyValue), Set()))
    parses("CONSTRUCT (x) SET x.age = 42 AND x:Person", Construct(Set(nodeBGP), Set(setKeyValue, setLabel), Set()))
    parses("CONSTRUCT (x)-[z]->(y) SET x:Person", Construct(Set(edgeBGP), Set(setLabel), Set()))
    parses("CONSTRUCT (x)-[z]->(y) SET x.age = 42", Construct(Set(edgeBGP), Set(setKeyValue), Set()))
    parses("CONSTRUCT (x)-[z]->(y) SET x.age = 42 AND x:Person", Construct(Set(edgeBGP), Set(setKeyValue, setLabel), Set()))
    parses("CONSTRUCT (x), (x)-[z]->(y) SET x:Person", Construct(longConstruct, Set(setLabel), Set()))
  }

  test("Parsing of Construct with RemoveClause") {
    implicit val pp = p.pConstruct

    parses("CONSTRUCT (x) REMOVE x:Person", Construct(Set(nodeBGP), Set(), Set(removeLabel)))
    parses("CONSTRUCT (x) REMOVE x.age", Construct(Set(nodeBGP), Set(), Set(removeKey)))
    parses("CONSTRUCT (x) REMOVE x.age AND x:Person", Construct(Set(nodeBGP), Set(), Set(removeKey, removeLabel)))
    parses("CONSTRUCT (x)-[z]->(y) REMOVE x:Person", Construct(Set(edgeBGP), Set(), Set(removeLabel)))
    parses("CONSTRUCT (x)-[z]->(y) REMOVE x.age ", Construct(Set(edgeBGP), Set(), Set(removeKey)))
    parses("CONSTRUCT (x)-[z]->(y) REMOVE x.age AND x:Person", Construct(Set(edgeBGP), Set(), Set(removeKey, removeLabel)))
    parses("CONSTRUCT (x), (x)-[z]->(y) REMOVE x:Person", Construct(longConstruct, Set(), Set(removeLabel)))
  }

  test("Parsing of Construct with SetClause and RemoveClause") {
    implicit val pp = p.pConstruct
    parses("CONSTRUCT (x) SET x.age = 42 AND x:Person REMOVE x.age AND x:Person", 
      Construct(Set(nodeBGP), Set(setKeyValue, setLabel), Set(removeKey, removeLabel)))
  }
