package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import org.softlang.s2s.core.S2STry
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

  private var count = 0

  // TODO: Multiple phases.
  def getNext(
      accepted: S2STry[Set[SHACLShape]] = Right(Set())
  ): Set[SHACLShape] =
    if heuristic.simpleShapes && count == 0 then
      count += 1
      generate
    else generate // TODO: Multiple phases depending on input.
