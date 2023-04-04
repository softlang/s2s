package org.softlang.s2s.core

import de.pseifer.shar.dl._
import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept

/** Encoding of available scopes. */
class Scopes(token: String, localTop: String, includeTop: Boolean):

  /** Rule for naming/appending for the differnt scopes. */
  private def makeScopeToken(scope: Scope): String = scope match
    case Scope.Input    => ""
    case Scope.Pattern  => token
    case Scope.Template => token ++ token

  /** Remove all scope tokens. */
  def removeScopeTokens(in: String): String =
    in.replaceAll(inputScopeToken, "")
      .replaceAll(patternScopeToken, "")
      .replaceAll(templateScopeToken, "")

  /** Get token for the respective scope. */
  def getToken(scope: Scope): String =
    scope match
      case Scope.Input    => inputScopeToken
      case Scope.Pattern  => patternScopeToken
      case Scope.Template => templateScopeToken

  /** The token for input scope. */
  val inputScopeToken: String = makeScopeToken(Scope.Input)

  /** The token for pattern scope. */
  val patternScopeToken: String = makeScopeToken(Scope.Pattern)

  /** The token for template scope. */
  val templateScopeToken: String = makeScopeToken(Scope.Template)

  private def doReplaceTop(c: Concept, theScope: Scope): Concept =
    Concept.map(c => if c == Top then top(theScope) else c, c)

  def replaceTop(ax: Axiom, leftScope: Scope, rightScope: Scope): Axiom =
    ax match
      case s @ Subsumption(_, _) => replaceTop(s, leftScope, rightScope)
      case e @ Equality(_, _)    => replaceTop(e, leftScope, rightScope)
      case a                     => a

  def replaceTop(
      sub: Subsumption,
      leftScope: Scope,
      rightScope: Scope
  ): Subsumption =
    Subsumption(doReplaceTop(sub.c, leftScope), doReplaceTop(sub.d, rightScope))

  def replaceTop(
      sub: Equality,
      leftScope: Scope,
      rightScope: Scope
  ): Equality =
    Equality(doReplaceTop(sub.c, leftScope), doReplaceTop(sub.d, rightScope))

  def replaceTop(
      shape: SimpleSHACLShape,
      leftScope: Scope,
      rightScope: Scope
  ): SimpleSHACLShape =
    SimpleSHACLShape(replaceTop(shape.axiom, leftScope, rightScope))

  def restoreTop(c: Concept): Concept =
    Concept.map(
      c =>
        if c == top(Scope.Input) || c == top(Scope.Pattern) || c == top(
            Scope.Template
          )
        then Top
        else c,
      c
    )

  def restoreTop(s: SimpleSHACLShape): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(restoreTop(s.axiom.c), restoreTop(s.axiom.d))
    )

  def restoreTop(ax: Axiom): Axiom = ax match
    case Subsumption(c, d) => Subsumption(restoreTop(c), restoreTop(d))
    case Equality(c, d)    => Equality(restoreTop(c), restoreTop(d))
    case a                 => a

  /** Get top for this scope. */
  def top(scope: Scope): Concept =
    if includeTop then
      NamedConcept(
        Iri
          .fromString(Iri.shar.expanded(localTop ++ getToken(scope)))
          .toOption
          .get
      )
    else Top
