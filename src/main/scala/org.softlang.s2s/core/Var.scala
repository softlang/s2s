package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.Concept
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.query.FilterPattern

/** A query variable. */
final case class Var(v: String) extends Showable:

  /** So that is implements Showable; state not required. */
  def show(implicit state: BackendState): String = showNB

  /** Show without requiring BackendState */
  def showNB: String = "?" ++ v

  def asConcept(implicit scopes: Scopes): Concept =
    NamedConcept(Iri.fromString(Iri.shar.expanded(v)).toOption.get).inScope(Scope.Variable)

  def asConceptComponent(filters: Set[FilterPattern], component: NamedConcept)
      (implicit scopes: Scopes): Option[Concept] =
    if ! filters.contains(FilterPattern.notC(this, component.c.dropScope)) then
      // TODO: This is unsafe and can easily wrongly merge concepts!
      val approx = component.c.getRaw.reverse.takeWhile(_ != '/').reverse
      Some(
        NamedConcept(Iri.fromString(Iri.shar.expanded(v ++ "_" ++ approx)).toOption.get)
          .inScope(Scope.Variable)
      )
    else
      None

  def asRoleObjectComponent(filters: Set[FilterPattern], component: NamedRole)
      (implicit scopes: Scopes): Option[(Concept, Concept)] =
    if ! filters.contains(FilterPattern.notP(this, component.r.dropScope)) then
      // TODO: This is unsafe and can easily wrongly merge concepts!
      val approx = component.r.getRaw.reverse.takeWhile(_ != '/').reverse
      Some((
        NamedConcept(Iri.fromString(Iri.shar.expanded(v ++ "_" ++ approx)).toOption.get)
          .inScope(Scope.Variable),
        NamedConcept(Iri.fromString(Iri.shar.expanded(v ++ "_o_" ++ approx)).toOption.get)
          .inScope(Scope.Variable)
      ))
    else
      None

  def asRoleComponent(filters: Set[FilterPattern], component: NamedRole)
      (implicit scopes: Scopes): Option[Concept] =
      asRoleObjectComponent(filters, component).map(_._1)

  def asObjectComponent(filters: Set[FilterPattern], component: NamedRole)
      (implicit scopes: Scopes): Option[Concept] =
      asRoleObjectComponent(filters, component).map(_._2)

  def toIri(implicit scopes: Scopes): Iri =
    this.asConcept.asInstanceOf[NamedConcept].c

  /** Is a fresh variable instantiated by Var.fresh(). */
  def isFresh: Boolean = v.contains(Var.freshToken)

object Var:
  def freshToken = "_"
  private var freshCounter = -1

  def fromIriUnsafe(i: Iri)(implicit scopes: Scopes): Var =
    Var(i.dropScopeVariableInternal(scopes).retracted(Iri.shar).get)

  def counterReset(): Unit =
    freshCounter = -1

  /** Generate a fresh variable. */
  def fresh(): Var =
    freshCounter += 1
    Var(freshToken ++ freshCounter.toString)
