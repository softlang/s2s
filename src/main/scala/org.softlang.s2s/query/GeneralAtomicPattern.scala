package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import org.softlang.s2s.core.NotAtomicError
import org.softlang.s2s.core.ShassTry
import org.softlang.s2s.core.Var

enum GeneralAtomicPattern extends Showable:
  def show(implicit state: BackendState): String =
    this match
      case LAC(is, io) => is.show(state) ++ " a " ++ io.show(state)
      case VAC(vs, io) => vs.show ++ " a " ++ io.show(state)
      case VAV(vs, vo) => vs.show ++ " a " ++ vo.show
      case LAV(is, vo) => is.show(state) ++ " a " ++ vo.show
      case LPL(is, ip, io) =>
        is.show(state) ++ " " ++ ip.show(state) ++ " " ++ io.show(state)
      case VPL(vs, ip, io) =>
        vs.show ++ " " ++ ip.show(state) ++ " " ++ io.show(state)
      case LPV(is, ip, vo) =>
        is.show(state) ++ " " ++ ip.show(state) ++ " " ++ vo.show
      case VPV(vs, ip, vo) =>
        vs.show ++ " " ++ ip.show(state) ++ " " ++ vo.show
      case LVL(is, vp, io) =>
        is.show(state) ++ " " ++ vp.show ++ " " ++ io.show(state)
      case VVL(vs, vp, io) =>
        vs.show ++ " " ++ vp.show ++ " " ++ io.show(state)
      case LVV(is, vp, vo) =>
        is.show(state) ++ " " ++ vp.show ++ " " ++ vo.show
      case VVV(vs, vp, vo) =>
        vs.show ++ " " ++ vp.show ++ " " ++ vo.show

  case LAC(is: Iri, io: Iri) //  x  a  C
  case VAC(vs: Var, io: Iri) // ?x  a  C
  case VAV(vs: Var, vo: Var) // ?x  a ?c
  case LAV(is: Iri, vo: Var) //  x  a ?c

  case LPL(is: Iri, ip: Iri, io: Iri) //  X  P  Y
  case VPL(vs: Var, ip: Iri, io: Iri) // ?x  P  Y
  case LPV(is: Iri, ip: Iri, vo: Var) //  X  P ?y
  case VPV(vs: Var, ip: Iri, vo: Var) // ?x  P ?y

  case LVL(is: Iri, vp: Var, io: Iri) //  X ?p  Y
  case VVL(vs: Var, vp: Var, io: Iri) // ?x ?p  Y
  case LVV(is: Iri, vp: Var, vo: Var) //  X ?p ?y
  case VVV(vs: Var, vp: Var, vo: Var) // ?x ?p ?y

  /** Convert to an atomic pattern. */
  def toAtomicPattern: ShassTry[AtomicPattern] =
    this match
      case LAC(is, io)     => Right(AtomicPattern.LAC(is, io))
      case VAC(vs, io)     => Right(AtomicPattern.VAC(vs, io))
      case LPL(is, ip, io) => Right(AtomicPattern.LPL(is, ip, io))
      case VPL(vs, ip, io) => Right(AtomicPattern.VPL(vs, ip, io))
      case LPV(is, ip, vo) => Right(AtomicPattern.LPV(is, ip, vo))
      case VPV(vs, ip, vo) => Right(AtomicPattern.VPV(vs, ip, vo))
      case _               => Left(NotAtomicError(this))
