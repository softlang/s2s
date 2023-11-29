package org.softlang.s2s.core

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._

extension (axiom: Axiom)
  /** Obtain the vocabulary of this axiom. */
  def vocabulary: Vocabulary =
    def findNominals() = axiom match
      case Subsumption(c, d)  => 
        var nominals: Set[Iri] = Set()
        Concept.foreach(ci => ci match
          case NominalConcept(i) => nominals += i
          case _ => ()
        , c)
        nominals
      case _ => Set()
    Vocabulary(
      variables = Set(), 
      concepts = axiom.concepts.map(NamedConcept(_)), 
      properties = axiom.properties.map(NamedRole(_)), 
      nominals = findNominals())

  /** Set scope for expressions in this axiom. */
  def inScope(scope: Scope)(implicit scopes: Scopes): Axiom = axiom match
    case Subsumption(c, d) => Subsumption(c.inScope(scope), d.inScope(scope))
    case Equality(c, d) => Equality(c.inScope(scope), d.inScope(scope))
    case Satisfiability(c) => Satisfiability(c.inScope(scope))
    case RoleSubsumption(r, p) => RoleSubsumption(r.inScope(scope), p.inScope(scope))

  /** Update Scopes from oldS to newS. */
  def updateScopes(oldS: Scopes, newS: Scopes): Axiom = axiom match
    case Subsumption(c, d) => Subsumption(c.updateScopes(oldS, newS), d.updateScopes(oldS, newS))
    case Equality(c, d) => Equality(c.updateScopes(oldS, newS), d.updateScopes(oldS, newS))
    case Satisfiability(c) => Satisfiability(c.updateScopes(oldS, newS))
    case RoleSubsumption(r, p) => RoleSubsumption(r.updateScopes(oldS, newS), p.updateScopes(oldS, newS))

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

  def isVariable: Boolean =
    // Only variables use the internal shar prefix.
    i.retracted(Iri.shar).isDefined

  /** This IRI contains a substring. */
  def contains(s: String): Boolean =
    i.toString.indexOf(s) != -1

  /** Update scope tokens from one Scopes to another. */
  def updateScopes(oldS: Scopes, newS: Scopes): Iri = 
    Iri.makeFromRawIri(oldS.updateScopeTokens(i.getRaw, newS)).toOption.get

  /** Get this IRI in the respective scope. */
  def inScope(scope: Scope)(implicit scopes: Scopes): Iri =
    // For variables, only apply the variable scope.
    if i.isVariable then
      if scope == Scope.Variable then
        i.getBase(scopes)
         .append(scopes.getToken(Scope.Variable))
      else i
    // For anything else, apply whatever scope is given.
    else
      scope match
        case Scope.None =>
          i.getBase(scopes)
            .append(scopes.getToken(Scope.None))
        case Scope.In =>
          i.getBase(scopes)
            .append(scopes.getToken(Scope.In))
        case Scope.Med =>
          i.getBase(scopes)
            .append(scopes.getToken(Scope.Med))
        case Scope.Out =>
          i.getBase(scopes)
            .append(scopes.getToken(Scope.Out))
        // Note: This never occurs. Perhaps it should be forbidden?
        case Scope.Variable =>
          i.getBase(scopes)
            .append(scopes.getToken(Scope.Variable))

  /** Drop all scoping for this IRI. */
  def dropScope(implicit scopes: Scopes): Iri =
    if i.isVariable then i else i.getBase(scopes)

  /** Also drop scopes for variables. Only for GCORE support. */
  def dropScopeVariableInternal(implicit scopes: Scopes): Iri =
    i.getBase(scopes)

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

  def dropScope(implicit scopes: Scopes): Concept = c match
    case NamedConcept(i) => NamedConcept(i.dropScope)
    case Existential(r, c) =>
      Existential(r.dropScope, c.dropScope)
    case Universal(r, c) =>
      Universal(r.dropScope, c.dropScope)
    case Union(c1, c2) =>
      Union(c1.dropScope, c2.dropScope)
    case Intersection(c1, c2) =>
      Intersection(c1.dropScope, c2.dropScope)
    case c => c

  def updateScopes(oldS: Scopes, newS: Scopes): Concept = c match
    case NamedConcept(i) => NamedConcept(i.updateScopes(oldS, newS))
    case Existential(r, c) =>
      Existential(r.updateScopes(oldS, newS), c.updateScopes(oldS, newS))
    case Universal(r, c) =>
      Universal(r.updateScopes(oldS, newS), c.updateScopes(oldS, newS))
    case Union(c1, c2) =>
      Union(c1.updateScopes(oldS, newS), c2.updateScopes(oldS, newS))
    case Intersection(c1, c2) =>
      Intersection(c1.updateScopes(oldS, newS), c2.updateScopes(oldS, newS))
    case c => c


extension (r: Role)
  def inScope(scope: Scope)(implicit scopes: Scopes): Role =
    r match
      case NamedRole(i) => NamedRole(i.inScope(scope))
      case Inverse(r)   => Inverse(r.inScope(scope))

  def dropScope(implicit scopes: Scopes): Role =
    r match
      case NamedRole(i) => NamedRole(i.dropScope)
      case Inverse(r)   => Inverse(r.dropScope)

  def updateScopes(oldS: Scopes, newS: Scopes): Role =
    r match
      case NamedRole(i) => NamedRole(i.updateScopes(oldS, newS))
      case Inverse(r)   => Inverse(r.updateScopes(oldS, newS))

/** Basic utility functions. */
object Util:

  /** List of 'eithers' to either of lists. */
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

  /** Sequence a list of option to option of list. */
  def sequence[A](l: List[Option[A]]): Option[List[A]] = l.foldLeft(Option(List.empty[A])) {
    case(Some(r), Some(v)) => Some(v :: r); 
    case(_, _) => None 
  }

  /** Construct IRI for testing purposes. */
  def forceIriUnsafe(s: String): Iri =
    Iri.fromString(s"<https://github.com/softlang/s2s/testing/$s>").toOption.get

  /** Compatibility mapping of characters, since ANTLR is somewhat system
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
      sin: Set[String],
      oneLine: Boolean = true,
      prefix: String = "",
      postfix: String = "",
      token: String = ",",
      indent: String = "  ",
      sorted: Boolean = false
  ): String =
    val s = if sorted then sin.toList.sorted else sin.toList
    if s.isEmpty then prefix ++ "∅"
    else if s.size == 1 then prefix ++ s.head ++ postfix
    else if oneLine then prefix ++ s.map(_.trim).mkString(token) ++ postfix
    else
      prefix ++ "\n" ++ s
        .map(indent ++ _.trim)
        .mkString(token ++ "\n") ++ postfix
