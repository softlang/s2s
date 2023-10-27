package org.softlang.s2s.core

/** Encoding of available scopes. */
class Scopes(token: String, in: Int, med: Int, out: Int, variable: Int):

  /** Rule for naming/appending for the different scopes. */
  private def makeScopeToken(scope: Scope): String = scope match
    case Scope.None    => ""
    case Scope.In    => token ++ in.toString
    case Scope.Med  => token ++ med.toString
    case Scope.Out => token ++ out.toString
    case Scope.Variable    => token ++ variable.toString

  /** Remove all scope tokens. */
  def removeScopeTokens(in: String): String =
    in.replaceAll(getToken(Scope.None), "")
      .replaceAll(getToken(Scope.In), "")
      .replaceAll(getToken(Scope.Med), "")
      .replaceAll(getToken(Scope.Out), "")
      .replaceAll(getToken(Scope.Variable), "")

  /** Replace internal scope token+ID with none, one, and two 'replaceWith'
   *  strings for in, med and out scope, respectively. */
  def prettyScopeTokens(in: String, replaceWith: String = token): String =
    in.replaceAll(getToken(Scope.In), "")
      .replaceAll(getToken(Scope.Variable), "")
      .replaceAll(getToken(Scope.Med), replaceWith)
      .replaceAll(getToken(Scope.Out), replaceWith ++ replaceWith)

  /** Get token for the respective scope. */
  def getToken(scope: Scope): String =
    makeScopeToken(scope)

