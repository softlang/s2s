package org.softlang.s2s.parser

import org.softlang.s2s.core._

import de.pseifer.shar.Shar
import de.pseifer.shar.dl.{Concept, Subsumption}
import de.pseifer.shar.parsing.ConceptParser

class ShapeParser(shar: Shar):

  private val cp = ConceptParser(shar.state)

  private def doParse(s: String): ShassTry[Concept] =
    cp.parse(s) match
      case Left(p)           => Left(UnparsableShapeError(p.show))
      case Right(c: Concept) => Right(c)
      case Right(c)          => Left(NotAShapeError(c))

  def parse(in: String): ShassTry[SimpleSHACLShape] =
    val inn = Util.compatMap(in)
    if inn.contains("⊑") then
      val target = inn.splitAt(inn.indexOf("⊑"))._1.trim
      val constraint = inn.splitAt(inn.indexOf("⊑"))._2.drop(1).trim

      for
        t <- doParse(target)
        c <- doParse(constraint)
        s <- SimpleSHACLShape.fromAxiom(Subsumption(t, c))
      yield s
    else Left(UnparsableShapeError(in))
