package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.Concept
import de.pseifer.shar.dl.NamedConcept

/** A query variable. */
final case class Var(v: String) extends Showable:
  def show(implicit state: BackendState): String = "?" + v

  def asConcept(implicit scopes: Scopes): Concept =
    NamedConcept(Iri.fromString(Iri.shar.expanded(v)).toOption.get).inScope(Scope.Variable)

  def toIri(implicit scopes: Scopes): Iri = 
    this.asConcept.asInstanceOf[NamedConcept].c

  /** Is a fresh variable instantiated by Var.fresh(). */
  def isFresh: Boolean = v.contains(Var.freshToken)

object Var:
  def freshToken = "?"
  private var freshCounter = -1

  def fromIriUnsafe(i: Iri)(implicit scopes: Scopes): Var = 
    Var(i.dropScopeVariableInternal(scopes).retracted(Iri.shar).get)

  def counterReset(): Unit =
    freshCounter = -1

  /** Generate a fresh variable. */
  def fresh(): Var =
    freshCounter += 1

    Var(freshToken ++ freshCounter.toString)
