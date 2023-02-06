package org.softlang.s2s.main

import org.softlang.s2s.analysis.ConfigurationComparison
import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration

/** Compare two configurations of the algorithm on structured test cases. */
object Compare:
  def run(): Unit =
    val compare = ConfigurationComparison(
      Configuration.paper,
      Configuration.og,
      // Perform 1000 trials per generator configuration.
      trials = 1,
      // Generate multiple results.
      stopAfterFirstResult = false,
      title1 = "Paper",
      title2 = "Implementation OG"
    )

    compare.input(
      """
      |CONSTRUCT {
      |  ?x a :B .
      |  ?x :q ?y . 
      |  ?y a :B
      |} WHERE {
      |  ?x a :A . 
      |  ?x :r ?y . 
      |  ?y a :A
      |}
    """.stripMargin('|'),
      Set()
    )

    // compare.input(
    //  """
    //  |CONSTRUCT {
    //  |  ?x a :A .
    //  |  ?y a :B .
    //  |  ?z a :C
    //  |} WHERE {
    //  |  ?x :p ?y .
    //  |  ?z :p ?z
    //  |}
    // """.stripMargin('|'),
    //  Set()
    // )

    compare.input(
      """
     |CONSTRUCT {
     |  ?x :r ?y .
     |  ?z a :D
     |} WHERE {
     |  ?x :r ?y .
     |  ?z a :D
     |}
    """.stripMargin('|'),
      Set(":D ⊑ ∃:l.:D")
    )
