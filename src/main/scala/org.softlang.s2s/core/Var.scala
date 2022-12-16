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

  /** Is a fresh variable instantiated by Var.fresh(). */
  def isFresh: Boolean = v.contains(Var.freshToken)

object Var:
  def freshToken = "?"
  private var freshCounter = -1

  /** Generate a fresh variable. */
  def fresh(): Var =
    freshCounter += 1

    if freshCounter > 999 then
      throw new RuntimeException(
        "Requesting exceedingly large number of variables. " +
          "This is probably not intended."
      )

    Var(freshToken ++ freshCounter.toString)
