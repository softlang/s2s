package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.Concept
import de.pseifer.shar.dl.NamedConcept

/** A query variable. */
final case class Var(v: String) extends Showable with AsConcept:
  def show(implicit state: BackendState): String = "?" + v

  def asConcept: Concept =
    NamedConcept(Iri.fromString(Iri.shar.expanded(v)).toOption.get)
