package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.inScope

class CandidateGenerator(voc: Vocabulary, optimize: Boolean)(implicit
    scopes: Scopes
) extends ShapeGenerator(voc, optimize):

  def axioms: Set[SimpleSHACLShape] =
    generate.map(scopes.replaceTop(_, Scope.Template, Scope.Template))
