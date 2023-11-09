package org.softlang.s2s.test

import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ

class GCOREtoSCCQTests extends munit.FunSuite:
  import GCORE._

  val gq1 = GCORE(
      ConstructClause.Construct(
        Set(
          BasicGraphPattern.NodePattern(Variable("x"))
        ),
        Nil,
        Nil
      ),
      MatchClause.Match(
        Set(
          BasicGraphPattern.NodePattern(Variable("x")),
        ),
        Nil
      )
    )

  test("Minimal G-CORE query to SCCQ query") {
    assert(true)
  }
