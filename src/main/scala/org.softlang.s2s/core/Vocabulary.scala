package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.Var

case class Vocabulary(
    variables: Set[Var],
    concepts: Set[NamedConcept],
    properties: Set[NamedRole],
    nominals: Set[Iri]
) extends Showable:

  def show(implicit state: BackendState): String =
    Util.formatSet(variables.map(_.show), postfix = " ∈ X, ") ++
      Util.formatSet(concepts.map(_.show), postfix = " ∈ C, ") ++
      Util.formatSet(properties.map(_.show), postfix = " ∈ P and ") ++
      Util.formatSet(nominals.map(_.show), postfix = " ∈ O")

  def contains(v: Var): Boolean = variables.contains(v)

  def contains(c: NamedConcept): Boolean = concepts.contains(c)

  def contains(p: NamedRole): Boolean = properties.contains(p)

  def intersect(voc: Vocabulary): Vocabulary = Vocabulary(
    variables.intersect(voc.variables),
    concepts.intersect(voc.concepts),
    properties.intersect(voc.properties),
    nominals.intersect(voc.nominals)
  )

  def union(voc: Vocabulary): Vocabulary = Vocabulary(
    variables.union(voc.variables),
    concepts.union(voc.concepts),
    properties.union(voc.properties),
    nominals.union(voc.nominals)
  )

  def diff(voc: Vocabulary): Vocabulary = Vocabulary(
    variables.diff(voc.variables),
    concepts.diff(voc.concepts),
    properties.diff(voc.properties),
    nominals.diff(voc.nominals)
  )

  /** Appends ' to name. */
  def triviallyRenamed: Vocabulary = Vocabulary(
    variables,
    concepts.map(_.renameIris("'").asInstanceOf[NamedConcept]),
    properties.map(_.renameIris("'").asInstanceOf[NamedRole]),
    nominals.map(_.rename("'"))
  )
