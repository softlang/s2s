package org.softlang.shass

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

  val shass = Shass(
    log = true,
    debug = true,
    prefix = ":",
    hidecolon = true
  )

  shass.run(q, Set())
