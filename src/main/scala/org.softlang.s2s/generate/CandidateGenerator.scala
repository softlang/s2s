package org.softlang.s2s.generate

import org.softlang.s2s.core.S2STry
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.ShapeHeuristic
import org.softlang.s2s.core.Vocabulary

import de.pseifer.shar.core.Iri

/** Candidate shape generator. */
class CandidateGenerator(
    voc: Vocabulary,
    heuristic: ShapeHeuristic,
    excludeTarget: Set[Iri] = Set()
)(implicit scopes: Scopes)
    extends ShapeGenerator(voc, heuristic, excludeTarget):

  private var count = 0

  // TODO: Multiple phases.
  def getNext(
      accepted: S2STry[Set[SHACLShape]] = Right(Set())
  ): Set[SHACLShape] =
    heuristic match
      case _: ShapeHeuristic.SimpleShapes if count == 0 =>
        count += 1
        generate
      case _: ShapeHeuristic.MediumProGS if count == 0 =>
        count += 1
        generate
      case _: ShapeHeuristic.NovaProGS if count == 0 =>
        count += 1
        generate
      case _ => Set()
      // TODO: Multiple phases depending on input.
      // TODO: Extend for phase-wise, optimize generation
      // of arbitrary DL SHACL shapes.
      // See also @ShapeGenerator
