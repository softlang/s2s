package org.softlang.s2s

def q: String =
  """
  CONSTRUCT {
    ?x a :B . ?z a :C
  } WHERE {
    ?x :p ?y . ?z :p ?u . ?x a :A . ?z a :A . ?y a :D . ?u a :D
  }
  """

def sin: Set[String] = Set()

@main def main: Unit =

  val s2s = Shapes2Shapes(
    log = true,
    debug = true,
    prefix = ":",
    hidecolon = true
  )

  s2s.run(q, Set())
