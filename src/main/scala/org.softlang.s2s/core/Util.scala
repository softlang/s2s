package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._

extension (i: Iri)

  /** Append some String to an IRI. */
  private def append(added: String): Iri =
    Iri
      .makeFromRawIri(i.getRaw ++ added)
      .toOption
      .get

  /** Return the IRI without any token. */
  private def getBase(scopes: Scopes): Iri =
    Iri.makeFromRawIri(scopes.removeScopeTokens(i.getRaw)).toOption.get

  private def isVariable: Boolean =
    // Only variables use the internal shar prefix.
    i.retracted(Iri.shar).isDefined

  /** Get this IRI in the respective scope. */
  def inScope(scope: Scope)(implicit scopes: Scopes): Iri =
    if i.isVariable then i
    else
      scope match
        case Scope.Pattern =>
          i.getBase(scopes)
            .append(scopes.patternScopeToken)
        case Scope.Template =>
          i.getBase(scopes)
            .append(scopes.templateScopeToken)
        case Scope.Input =>
          i.getBase(scopes)
            .append(scopes.inputScopeToken)

extension (c: Concept)
  def inScope(scope: Scope)(implicit scopes: Scopes): Concept = c match
    case NamedConcept(i) => NamedConcept(i.inScope(scope))
    case Existential(r, c) =>
      Existential(r.inScope(scope), c.inScope(scope))
    case Universal(r, c) =>
      Universal(r.inScope(scope), c.inScope(scope))
    case Union(c1, c2) =>
      Union(c1.inScope(scope), c2.inScope(scope))
    case Intersection(c1, c2) =>
      Intersection(c1.inScope(scope), c2.inScope(scope))
    case c => c

extension (r: Role)
  def inScope(scope: Scope)(implicit scopes: Scopes): Role =
    r match
      case NamedRole(i) => NamedRole(i.inScope(scope))
      case Inverse(r)   => Inverse(r.inScope(scope))

/** Basic utility functions. */
object Util:

  /** List of eithers to either of lists. */
  def flipEither[T1, T2](
      eithers: List[Either[T1, T2]]
  ): Either[List[T1], List[T2]] =
    eithers.partitionMap(identity) match {
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts)
    }

  /** List of eithers to either of first left or list of rights. */
  def flipEitherHead[T1, T2](
      eithers: List[Either[T1, T2]]
  ): Either[T1, List[T2]] =
    flipEither(eithers).left.map(_.head)

  /** Compatbility mapping of characters, since ANTLR is somewhat system
    * dependent with it's encodings.
    */
  def compatMap(s: String): String =
    s
      .replaceAll("⊤", "#t")
      .replaceAll("⊥", "#f")
      .replaceAll("∃", "#E")
      .replaceAll("∀", "#A")
      .replaceAll("⊓", "&")
      .replaceAll("⊔", "|")

  def formatSet(
      s: Set[String],
      oneline: Boolean = true,
      prefix: String = "",
      postfix: String = "",
      token: String = ",",
      indent: String = "  "
  ): String =
    if s.isEmpty then prefix ++ "∅"
    else if s.size == 1 then prefix ++ s.head ++ postfix
    else if oneline then prefix ++ s.map(_.trim).mkString(token) ++ postfix
    else
      prefix ++ "\n" ++ s
        .map(indent ++ _.trim)
        .mkString(token ++ "\n") ++ postfix
