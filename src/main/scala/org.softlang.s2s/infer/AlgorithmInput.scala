package org.softlang.s2s.infer

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.dl.Concept

import org.softlang.s2s.core._
import org.softlang.s2s.core.{inScope => cinScope}

import org.softlang.s2s.query.AtomicPatterns
import org.softlang.s2s.query.AtomicPattern
import org.softlang.s2s.query.vocabulary
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.FilterPattern
import org.softlang.s2s.parser.JsonLDParser

object AlgorithmInput:

  def fromSetOfShapesGCORE(
      q: GCORE,
      shapes: Set[SHACLShape],
      inputScopes: Scopes
  ): AlgorithmInput.GCOREAxioms =
    AlgorithmInput.GCOREAxioms(q, Axioms(shapes.map(_.axiom), inputScopes))

  def fromSetOfShapesSCCQ(
      q: SCCQ,
      shapes: Set[SHACLShape],
      inputScopes: Scopes
  ): AlgorithmInput.SCCQAxioms =
    AlgorithmInput.SCCQAxioms(q, Axioms(shapes.map(_.axiom), inputScopes))

/** Specifies the possible inputs for the Algorithm. */
enum AlgorithmInput:

  /** A SCCQ query and a set of SimpleSHACLShapes. */
  case SCCQSimpleSHACL(
      q: SCCQ,
      axioms: Set[SimpleSHACLShape] = Set(),
      inputScopes: Scopes
  )

  /** A SCCQ query and a set of Axioms. */
  case SCCQAxioms(q: SCCQ, axioms: Axioms)

  /** A GCORE query and a set of Axioms. */
  case GCOREAxioms(q: GCORE, axioms: Axioms)

  // Various functions dealing with these inputs.

  /** The scopes associated with this input. */
  implicit val composeScopes: Scopes = this match
    case SCCQSimpleSHACL(_, _, s) => s
    case SCCQAxioms(_, a)  => a.scopes // TODO composeScopes, but also rescope?
    case GCOREAxioms(_, a) => a.scopes // TODO composeScopes, but also rescope?

  /** Get the template of the input query. */
  val template: S2STry[AtomicPatterns] = this match
    case SCCQSimpleSHACL(q, _, _) => Right(q.template.inScope(Scope.Out))
    case SCCQAxioms(q, _)         => Right(q.template.inScope(Scope.Out))
    case GCOREAxioms(q, _) =>
      q.toSCCQ.map(_.template) match
        case None =>
          Left(
            UnsupportedQueryError(
              q,
              details = "This GCORE query can not be converted to SPARQL."
            )
          )
        case Some(t) =>
          val upt = t.filter(p =>
            p match
              case AtomicPattern.VAC(_, GCORE.node) => false
              case AtomicPattern.VAC(_, GCORE.edge) => false
              case _                                => true
          )
          // TODO: Tis' correct?
          Right(upt.inScope(Scope.Out))
    // Right(t.inScope(Scope.Out))

  /** Get the pattern of the input query. */
  val pattern: S2STry[AtomicPatterns] = this match
    case SCCQSimpleSHACL(q, _, _) => Right(q.pattern.inScope(Scope.Med))
    case SCCQAxioms(q, _)         => Right(q.pattern.inScope(Scope.Med))
    case GCOREAxioms(q, _) =>
      q.toSCCQ.map(_.pattern) match
        case None =>
          Left(
            UnsupportedQueryError(
              q,
              details = "This GCORE query can not be converted to SPARQL."
            )
          )
        case Some(p) =>
          val upp = p.filter(i =>
            i match
              case AtomicPattern.VAC(_, GCORE.node) => false
              case AtomicPattern.VAC(_, GCORE.edge) => false
              case _                                => true
          )
          // TODO: Tis' correct?
          Right(upp.inScope(Scope.Med))
    // Right(p.inScope(Scope.Med))

  /** Node variables of a GCORE query. */
  val nodeVariables: Set[Var] = this match
    case _: SCCQSimpleSHACL => Set()
    case _: SCCQAxioms      => Set()
    case GCOREAxioms(q, _)  => q.nodeVariables

  /** Node variables that are not blank of a GCORE query. */
  val nodeVariablesNonBlank: Set[Var] = this match
    case _: SCCQSimpleSHACL => Set()
    case _: SCCQAxioms      => Set()
    case GCOREAxioms(q, _)  => q.nodeVariables.filter(!_.isBlank)

  /** Left node variables of a GCORE query. */
  val leftNodeVariables: Set[Var] = this match
    case _: SCCQSimpleSHACL => Set()
    case _: SCCQAxioms      => Set()
    case GCOREAxioms(q, _)  => q.leftNodeVariables

  /** Right node variables of a GCORE query. */
  val rightNodeVariables: Set[Var] = this match
    case _: SCCQSimpleSHACL => Set()
    case _: SCCQAxioms      => Set()
    case GCOREAxioms(q, _)  => q.rightNodeVariables

  /** Edge variables of a GCORE query. */
  val edgeVariables: Set[Var] = this match
    case _: SCCQSimpleSHACL => Set()
    case _: SCCQAxioms      => Set()
    case GCOREAxioms(q, _)  => q.edgeVariables

  /** Edge variables that are not blanks of a GCORE query. */
  val edgeVariablesNonBlank: Set[Var] = this match
    case _: SCCQSimpleSHACL => Set()
    case _: SCCQAxioms      => Set()
    case GCOREAxioms(q, _)  => q.edgeVariables.filter(!_.isBlank)

  /** Return input constraints as axioms. */
  val shapeAxioms: Axioms = this match
    case SCCQSimpleSHACL(_, s, _) => Axioms(s.map(_.axiom), composeScopes)
    case SCCQAxioms(_, ax)        => ax
    case GCOREAxioms(_, ax)       => ax

  /** Get shapes for extension steps; use 'convert', if not explicit. */
  def extensionShapes(
      converted: () => S2STry[Set[SHACLShape]]
  ): S2STry[Set[SHACLShape]] =
    this match
      case SCCQSimpleSHACL(_, s, _) => Right(s.map(_.asInstanceOf[SHACLShape]))
      case SCCQAxioms(_, _)         => converted()
      case GCOREAxioms(_, _)        => converted()

  /** True if the input is a ECCQ query mapped from G-CORE or G-CORE. */
  val isECCQ: Boolean =
    this match
      case SCCQSimpleSHACL(q, _, _) => q.isECCQ
      case SCCQAxioms(q, _)         => q.isECCQ
      case GCOREAxioms(_, _)        => true

  val isRETURN: Boolean =
    this match
      case SCCQSimpleSHACL(_, _, _) => false
      case SCCQAxioms(_, _)         => false
      case GCOREAxioms(q, _)        => q.ret.isDefined

  private val queryVocabulary: Vocabulary =
    this match
      case SCCQSimpleSHACL(q, _, _) => q.vocabulary
      case SCCQAxioms(q, _)         => q.vocabulary
      case GCOREAxioms(q, _) =>
        q.toSCCQ
          .map(_.vocabulary)
          .getOrElse(Vocabulary.empty)

  /** Get the full vocabulary of the input. */
  val vocabulary: Vocabulary =
    queryVocabulary.union(shapeAxioms.vocabulary)

  /** Get the vocabulary of only the input. */
  val vocabularyIn: Vocabulary =
    pattern
      .map(_.vocabulary)
      .getOrElse(Vocabulary.empty)
      .union(shapeAxioms.vocabulary.withoutVariableConcepts)

  /** Get the filter pattern for this query. */
  val filters: Set[FilterPattern] =
    this match
      case SCCQSimpleSHACL(q, _, _) => q.eccq.map(_.filter).getOrElse(Set())
      case SCCQAxioms(q, _)         => q.eccq.map(_.filter).getOrElse(Set())
      case GCOREAxioms(q, _) =>
        q.toSCCQ
          .map(_.eccq.get.filter)
          .getOrElse(Set())

  /** Concepts in out-scope that exist in G-CORE result. */
  val outConcepts: Set[Concept] =
    this match
      case GCOREAxioms(q, _) =>
        val ic = vocabularyIn.concepts.map(_.cinScope(Scope.Out))
        val fc = FilterPattern
          .removalVocabulary(filters)
          .concepts
          .map(_.cinScope(Scope.Out))
        ic.diff(fc)
      case _ => Set()

  /** Format the query for storing to a file. */
  def formatQuery(implicit state: BackendState): String = this match
    case SCCQSimpleSHACL(q, _, _) =>
      state.prefixes.toSPARQL() ++ "\n" ++ q.show
    case SCCQAxioms(q, _) =>
      state.prefixes.toSPARQL() ++ "\n" ++ q.show
    case GCOREAxioms(q, _) =>
      state.prefixes.toSPARQL() ++ "\n" ++ q.toSCCQ.map(_.show).getOrElse("")

  /** Format the shapes for storing to a file. */
  def formatShapes: S2STry[String] = this match
    case SCCQSimpleSHACL(_, shacl, _) =>
      JsonLDParser.unparse(shacl.map(_.dropScopeS))
    case SCCQAxioms(_, ax)  => JsonLDParser.unparse(ax.map(_.dropScope))
    case GCOREAxioms(_, ax) => JsonLDParser.unparse(ax.map(_.dropScope))

  /** Get the respective scopes. */
  def getScopes: Scopes = this match
    case SCCQSimpleSHACL(_, _, s) => s
    case SCCQAxioms(_, ax)        => ax.scopes
    case GCOREAxioms(_, ax)       => ax.scopes

  /** Write input to log. */
  def log(log: Log)(implicit state: BackendState): Unit = this match
    case SCCQSimpleSHACL(q, s, _) =>
      log.info("q", q.show)
      log.debug("Σ(q)", q.vocabulary.show)
      log.info("S_in", s.map(_.show).toList)
    case SCCQAxioms(q, a) =>
      log.info("q", q.show)
      log.debug("Σ(q)", q.vocabulary.show)
      log.info("S_in", a.toSet.map(_.show).toList)
    case GCOREAxioms(q, a) =>
      log.info("q", q.show)
      log.info("map(q)", q.toSCCQ.map(_.show).getOrElse(""))
      log.debug("Σ(q)", q.toSCCQ.map(_.vocabulary.show).getOrElse(""))
      log.info("S_in", a.toSet.map(_.show).toList)
