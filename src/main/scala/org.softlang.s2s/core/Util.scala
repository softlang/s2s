package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._

extension (i: Iri)
  def rename(s: String): Iri =
    Iri
      .makeFromRawIri(i.getRaw ++ s)
      .toOption
      .get

extension (c: Concept)
  def renameIris(s: String): Concept =
    c match
      case Top               => Top
      case Bottom            => Bottom
      case NominalConcept(i) => NominalConcept(i.rename(s))
      case NamedConcept(i)   => NamedConcept(i.rename(s))
      case Existential(r, c) => Existential(r.renameIris(s), c.renameIris(s))
      case Universal(r, c)   => Universal(r.renameIris(s), c.renameIris(s))
      case Union(c1, c2)     => Union(c1.renameIris(s), c2.renameIris(s))
      case Intersection(c1, c2) =>
        Intersection(c1.renameIris(s), c2.renameIris(s))
      case c =>
        throw new RuntimeException(
          "DL construct not supported in rename! (" ++ c.toString ++ ")"
        )

extension (r: Role)
  def renameIris(s: String): Role =
    r match
      case NamedRole(i) => NamedRole(i.rename(s))
      case Inverse(r)   => Inverse(r.renameIris(s))

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
