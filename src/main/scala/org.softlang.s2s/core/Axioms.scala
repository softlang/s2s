package org.softlang.s2s.core

import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.dl.Concept
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import de.pseifer.shar.dl.Role

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.reasoning.DLReasoner
import de.pseifer.shar.reasoning.AxiomSet

class Axioms(
  private val axioms: Set[Axiom],
  val scopes: Scopes) extends Showable:

  /** Join with other axioms. */
  def join(others: Axioms): Axioms =
    Axioms.join(this, others)

  /** Test, if the axioms are empty. */
  def isEmpty: Boolean = axioms.isEmpty

  /** Get axioms as a raw set. */
  def toSet: Set[Axiom] = axioms

  /** Make a shar AxiomSet. */
  def toAxiomSet: AxiomSet = AxiomSet(axioms)

  /** Get all concepts. */
  def concepts: Set[Concept] = axioms.flatMap(_.concepts).toSet.map(NamedConcept(_))

  /** Get all properties. */
  def properties: Set[Role] = axioms.flatMap(_.properties).toSet.map(NamedRole(_))

  /** Get the vocabulary of the axioms. */
  def vocabulary: Vocabulary = 
    axioms.map(_.vocabulary).foldLeft(Vocabulary.empty)(_.union(_))
  
  // Reasoner instances for this set of shapes, that are instantiated
  // with configs when first required. Usually, only one instance
  // is used.
  private var reasoners: Map[Configuration, DLReasoner] = Map()

  /** Check, using reasoning, whether 'ax' is entailed. */
  def entails(config: Configuration)(ax: Axiom): Boolean =
    // Setup reasoner on first attempt.
    if !reasoners.contains(config) then
      reasoners = reasoners.updated(config, config.reasoner.create)
      reasoners.get(config).get.addAxioms(toAxiomSet)
    reasoners.get(config).get.prove(ax)

  /** Check, via set inclusion, whether 'ax' is included. */
  def contains(ax: Axiom): Boolean = axioms.contains(ax)

  def show(implicit state: BackendState): String = show(", ")

  def show(token: String)(implicit state: BackendState): String =
    axioms.map(_.show(state)).mkString(token)

  /** Map a function on all concepts. */
  def map(f: Concept => Concept): Axioms = 
    import de.pseifer.shar.dl._
    Axioms(axioms.map { m =>
      m match
        case Subsumption(c, d)            => Subsumption(f(c), f(d))
        case Equality(c, d)               => Equality(f(c), f(d))
        case Satisfiability(c)            => Satisfiability(f(c))
        case rsub @ RoleSubsumption(_, _) => rsub
    },
    scopes)

  def canEqual(a: Any) = a.isInstanceOf[Axioms]

  override def equals(that: Any): Boolean =
    that match
      case that: Axioms =>
        this.axioms == that.axioms
      case _ => false

  override def hashCode: Int =
    this.axioms.hashCode


object Axioms:

  /** Join two sets of axioms, right-biased scopes. */
  def join(lhs: Axioms, rhs: Axioms): Axioms =
    Axioms(rhs.axioms.union(lhs.axioms.map(_.updateScopes(lhs.scopes, rhs.scopes))), rhs.scopes)

  /** Construct an empty axioms. */
  def empty(config: Configuration): Axioms = Axioms(Set(), Scopes.default(config.renameToken))




