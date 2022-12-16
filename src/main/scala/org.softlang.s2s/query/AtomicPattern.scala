package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import de.pseifer.shar.dl.NominalConcept
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.inScope

/** An atomic pattern of a SCCQ.
  */
enum AtomicPattern extends Showable:

  def show(implicit state: BackendState): String =
    toGeneralAtomicPattern.show(state)

  case LAC(is: Iri, io: Iri) //  x  a  C
  case VAC(vs: Var, io: Iri) // ?x  a  C
  case LPL(is: Iri, ip: Iri, io: Iri) //  X  P  Y
  case VPL(vs: Var, ip: Iri, io: Iri) // ?x  P  Y
  case LPV(is: Iri, ip: Iri, vo: Var) //  X  P ?y
  case VPV(vs: Var, ip: Iri, vo: Var) // ?x  P ?y

  /** Convert to a general atomic pattern. */
  def toGeneralAtomicPattern: GeneralAtomicPattern =
    this match
      case LAC(is, io)     => GeneralAtomicPattern.LAC(is, io)
      case VAC(vs, io)     => GeneralAtomicPattern.VAC(vs, io)
      case LPL(is, ip, io) => GeneralAtomicPattern.LPL(is, ip, io)
      case VPL(vs, ip, io) => GeneralAtomicPattern.VPL(vs, ip, io)
      case LPV(is, ip, vo) => GeneralAtomicPattern.LPV(is, ip, vo)
      case VPV(vs, ip, vo) => GeneralAtomicPattern.VPV(vs, ip, vo)

  /** Change the Scope of this pattern. */
  def inScope(scope: Scope)(implicit scopes: Scopes): AtomicPattern = this match
    case LAC(is, io)     => LAC(is, io.inScope(scope))
    case VAC(vs, io)     => VAC(vs, io.inScope(scope))
    case LPL(is, ip, io) => LPL(is, ip.inScope(scope), io)
    case VPL(vs, ip, io) => VPL(vs, ip.inScope(scope), io)
    case LPV(is, ip, vo) => LPV(is, ip.inScope(scope), vo)
    case VPV(vs, ip, vo) => VPV(vs, ip.inScope(scope), vo)

  /** */
  def isPropertyPattern: Boolean =
    this match
      case LAC(is, io) => false
      case VAC(vs, io) => false
      case _           => true

  /** Get all nominals in this pattern. */
  def nominals: Set[Iri] =
    this match
      case LAC(is, io)     => Set(is)
      case LPL(is, ip, io) => Set(is, io)
      case VPL(vs, ip, io) => Set(io)
      case LPV(is, ip, vo) => Set(is)
      case _               => Set()

  /** Get all variables in this pattern. */
  def variables: Set[Var] =
    this match
      case VAC(vs, io)     => Set(vs)
      case VPL(vs, ip, io) => Set(vs)
      case LPV(is, ip, vo) => Set(vo)
      case VPV(vs, ip, vo) => Set(vs, vo)
      case _               => Set()

  /** Get all properties in this pattern. */
  def properties: Set[NamedRole] =
    this match
      case LPL(_, ip, _) => Set(NamedRole(ip))
      case VPL(_, ip, _) => Set(NamedRole(ip))
      case LPV(_, ip, _) => Set(NamedRole(ip))
      case VPV(_, ip, _) => Set(NamedRole(ip))
      case _             => Set()

  /** Get all concepts in this pattern. */
  def concepts: Set[NamedConcept] =
    this match
      case LAC(_, io) => Set(NamedConcept(io))
      case VAC(_, io) => Set(NamedConcept(io))
      case _          => Set()

  /** Apply a variable mapping to a atomic pattern. */
  def mappedWith(mapping: Map[Var, Var]): AtomicPattern =
    def getMapping(v: Var): Var =
      if mapping.contains(v) then mapping(v) else v
    this match
      case LAC(is, io)     => LAC(is, io)
      case VAC(vs, io)     => VAC(getMapping(vs), io)
      case LPL(is, ip, io) => LPL(is, ip, io)
      case VPL(vs, ip, io) => VPL(getMapping(vs), ip, io)
      case LPV(is, ip, vo) => LPV(is, ip, getMapping(vo))
      case VPV(vs, ip, vo) => VPV(getMapping(vs), ip, getMapping(vo))
