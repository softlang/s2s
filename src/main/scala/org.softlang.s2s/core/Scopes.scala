package org.softlang.s2s.core

import de.pseifer.shar.dl._
import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept

/** Encoding of available scopes. */
class Scopes(token: String):

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
