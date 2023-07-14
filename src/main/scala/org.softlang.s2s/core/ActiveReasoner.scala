package org.softlang.s2s.core

import uk.ac.manchester.cs.jfact.JFactFactory
import openllet.owlapi.OpenlletReasonerFactory
import de.pseifer.shar.reasoning._

enum ActiveReasoner:
  case Hermit
  case Jfact
  case Openllet

  override def toString: String = this match
    case Hermit   => "hermit"
    case Jfact    => "jfact"
    case Openllet => "openllet"

  /** Instantiate the active reasoner. */
  def create: DLReasoner = this match
    case Hermit =>
      HermitReasoner(
        configuration = HermitConfiguration(
          existentialStrategy = HermitExistentialStrategy.IndividualReuse
        )
      )
    case Jfact =>
      OwlApiReasoner(JFactFactory())
    case Openllet =>
      OwlApiReasoner(OpenlletReasonerFactory())

object ActiveReasoner:

  /** Guess reasoner from string, default to HermiT. */
  def fromString(s: String): ActiveReasoner = s.toLowerCase() match
    case "jfact" | "fact" | "fact++"       => ActiveReasoner.Jfact
    case "openllet" | "openlet" | "pellet" => ActiveReasoner.Openllet
    case _                                 => ActiveReasoner.Hermit
