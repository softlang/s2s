package org.softlang.s2s.infer

import de.pseifer.shar.core.BackendState

import org.softlang.s2s.core._
import org.softlang.s2s.query.AtomicPatterns
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope

/** Specifies the possible inputs for the Algorithm. */
enum AlgorithmInput:

  /** A SCCQ query and a set of SimpleSHACLShapes. */
  case SCCQSimpleSHACL(q: SCCQ, axioms: Set[SimpleSHACLShape] = Set(), inputScopes: Scopes)

  /** A SCCQ query and a set of Axioms. */
  case SCCQAxioms(q: SCCQ, axioms: Axioms)

  /** A GCORE query and a set of Axioms. */
  case GCOREAxioms(q: GCORE, axioms: Axioms)

  // Various functions dealing with these inputs.

  /** The scopes associated with this input. */
  implicit val scopes: Scopes = this match
    case SCCQSimpleSHACL(_, _, s) => s
    case SCCQAxioms(_, a) => a.scopes
    case GCOREAxioms(_, a) => a.scopes

  /** Get the template of the input query. */
  def template(extraClauses: Set[GCORE.SetClause]): S2STry[AtomicPatterns] = this match
    case SCCQSimpleSHACL(q, _, _) => Right(q.template.inScope(Scope.Out))
    case SCCQAxioms(q, _) => Right(q.template.inScope(Scope.Out))
    case GCOREAxioms(q, _) => q.sccqTemplate(extraClauses) match
      case None => Left(
        UnsupportedQueryError(q, details = "This GCORE query can not be converted to SPARQL.")
      )
      case Some(t) => Right(t.inScope(Scope.Out))

  /** Get the pattern of the input query. */
  def pattern: S2STry[AtomicPatterns] = this match
    case SCCQSimpleSHACL(q, _, _) =>  Right(q.pattern.inScope(Scope.Med))
    case SCCQAxioms(q, _) => Right(q.pattern.inScope(Scope.Med))
    case GCOREAxioms(q, _) => q.sccqPattern match
      case None => Left(
        UnsupportedQueryError(q, details = "This GCORE query can not be converted to SPARQL.")
      )
      case Some(p) => Right(p.inScope(Scope.Med))

  /** Return input constraints as axioms. */
  def shapeAxioms: Axioms = this match
    case SCCQSimpleSHACL(_, s,  _) => Axioms(s.map(_.axiom), scopes)
    case SCCQAxioms(_, ax) => ax
    case GCOREAxioms(_, ax) => ax

  /** Get shapes for extension steps; use 'convert', if not explicit. */
  def extensionShapes(converted: () => S2STry[Set[SHACLShape]]): S2STry[Set[SHACLShape]] = 
    this match
      case SCCQSimpleSHACL(_, s, _) => Right(s.map(_.asInstanceOf[SHACLShape]))
      case SCCQAxioms(_, _) => converted()
      case GCOREAxioms(_, _) => converted()

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
      log.debug("Σ(q)", q.toSCCQ(Set()).map(_.vocabulary.show).getOrElse(""))
      log.info("S_in", a.toSet.map(_.show).toList)

