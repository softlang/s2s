package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.inScope

class CandidateGenerator(
    voc: Vocabulary,
    optimize: Boolean,
    proxyFamily: Boolean,
    simple: Boolean
)(implicit scopes: Scopes)
    extends ShapeGenerator(voc, optimize, proxyFamily, simple):

  def axioms: Set[SHACLShape] = generate
