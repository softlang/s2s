package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.ShapeHeuristic
import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.inScope

class CandidateGenerator(
    voc: Vocabulary,
    heuristic: ShapeHeuristic
)(implicit scopes: Scopes)
    extends ShapeGenerator(voc, heuristic):

  def axioms: Set[SHACLShape] = generate
