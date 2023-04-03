package org.softlang.s2s.main

import org.softlang.s2s.analysis.ConfigurationComparison
import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration

/** Compare two configurations of the algorithm on structured test cases. */
object Compare:
  def run(): Unit =
    val compare = ConfigurationComparison(
      Configuration.paper,
      Configuration.paper
        .copy(
          useNamespacedTop = true
        ),
      // Perform 1000 trials per generator configuration.
      trials = 10000,
      // Generate multiple results.
      stopAfterFirstResult = true,
      title1 = "Paper",
      title2 = "Paper + Namespaced Top"
    )

    // compare.structured()

    compare.input(
      """
      |CONSTRUCT { 
      | ?v2 a :C4 . ?v2 :p4 ?v1 . ?v1 :p3 ?v2 . :a1 :p3 :a2 
      |} WHERE { 
      | ?v1 :p2 ?v2
      |}
    """.stripMargin('|'),
      Set()
    )

    /*

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
     */
