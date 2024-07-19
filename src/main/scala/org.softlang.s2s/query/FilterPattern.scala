package org.softlang.s2s.query

import de.pseifer.shar.core.Iri
import org.softlang.s2s.core.Var

enum FilterPattern:
  case notC(v: Var, c: Iri) // x !C
  case notP(v: Var, p: Iri) // x !p
