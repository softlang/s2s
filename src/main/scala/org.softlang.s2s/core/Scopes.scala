package org.softlang.s2s.core

/** Encoding of available scopes. */
class Scopes(token: String):

  /** Rule for naming/appending for the differnt scopes. */
  private def makeScopeToken(scope: Scope): String = scope match
    case Scope.Input    => token ++ token
    case Scope.Pattern  => token
    case Scope.Template => ""

  /** Remove all scope tokens. */
  def removeScopeTokens(in: String): String =
    in.replaceAll(inputScopeToken, "")
      .replaceAll(patternScopeToken, "")
      .replaceAll(templateScopeToken, "")

  /** The token for input scope. */
  val inputScopeToken: String = makeScopeToken(Scope.Input)

  /** The token for pattern scope. */
  val patternScopeToken: String = makeScopeToken(Scope.Pattern)

  /** The token for template scope. */
  val templateScopeToken: String = makeScopeToken(Scope.Template)
