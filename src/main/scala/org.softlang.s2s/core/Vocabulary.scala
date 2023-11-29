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

  /** True, if the vocabulary contains the Variable. */
  def contains(v: Var): Boolean = variables.contains(v)

  /** True, if the vocabulary contains the NamedConcept. */
  def contains(c: NamedConcept): Boolean = concepts.contains(c)

  /** True, if the vocabulary contains the NamedRole. */
  def contains(p: NamedRole): Boolean = properties.contains(p)

  /** Set intersection of two vocabularies (component-wise). */
  def intersect(voc: Vocabulary): Vocabulary = Vocabulary(
    variables.intersect(voc.variables),
    concepts.intersect(voc.concepts),
    properties.intersect(voc.properties),
    nominals.intersect(voc.nominals)
  )

  /** Set union of two vocabularies (component-wise). */
  def union(voc: Vocabulary): Vocabulary = Vocabulary(
    variables.union(voc.variables),
    concepts.union(voc.concepts),
    properties.union(voc.properties),
    nominals.union(voc.nominals)
  )

  /** Set difference of two vocabularies (component-wise). */
  def diff(voc: Vocabulary): Vocabulary = Vocabulary(
    variables.diff(voc.variables),
    concepts.diff(voc.concepts),
    properties.diff(voc.properties),
    nominals.diff(voc.nominals)
  )

  /** True, if this vocabulary contains a specific name (String) somewhere. */
  def contains(s: String): Boolean =
    variables.exists(v => v.v.indexOf(s) != -1) || concepts.exists(c =>
      c.c.contains(s)
    ) || properties.exists(p => p.r.contains(s)) || nominals.exists(n =>
      n.contains(s)
    )

object Vocabulary:
  /** An empty vocabulary. */
  def empty: Vocabulary = 
    Vocabulary(Set(), Set(), Set(), Set())
