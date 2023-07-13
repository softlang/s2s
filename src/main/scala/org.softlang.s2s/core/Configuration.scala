package org.softlang.s2s.core

import uk.ac.manchester.cs.jfact.JFactFactory

enum ActiveReasoner:
  case Hermit
  case Jfact
  case Openllet

  override def toString: String = this match
    case Hermit   => "hermit"
    case Jfact    => "jfact"
    case Openllet => "openllet"

object ActiveReasoner:

  /** Construct active reasoner from string, default to Hermit. */
  def fromString(s: String): ActiveReasoner = s.toLowerCase() match
    case "jfact"    => ActiveReasoner.Jfact
    case "openllet" => ActiveReasoner.Openllet
    case _          => ActiveReasoner.Hermit

/** A set of configurations for S2S. */
case class Configuration(
    // *****************
    // *** Algorithm ***
    // *****************

    // Use a proxy for the family of form C ⊑ ∀p.P, instead of ignoring them.
    proxyFamily: Boolean,

    // Allow arbitrary (ALCHOI) SHACL shapes, instead of just SimpleSHACL.
    arbitraryShapes: Boolean,

    // ********************
    // *** User Options ***
    // ********************

    // Use the JFact reasoner, instead of HermiT.
    activeReasoner: ActiveReasoner,

    // Timeout for reasoning attempts (in milliseconds).
    timeout: Long,

    // Times the reasoning is allowed to restart after timeout.
    retry: Int,

    // Optimize candidate generation.
    optimizeCandidates: Boolean,

    // Token to append (if autoRename is set).
    renameToken: String,

    // Standard predefined prefix.
    prefix: String,

    // Print log to stdout when calling Shapes2Shapes.run.
    log: Boolean,

    // Generate more detailed, debugging output.
    debug: Boolean,

    // More formal notation in output when using ':' as a prefix.
    hidecolon: Boolean,

    // Replace the shar: prefix (variable concepts) with '?'.
    prettyVariableConcepts: Boolean,

    // Print output shapes (outside of log) to output.
    printOutput: Boolean
)

/** Some preset configurations. */
object Configuration:

  /** Default configuration. This is mainly for internal use.
    */
  def default: Configuration = Configuration(
    // The following can be set by users via CLI
    // and are overwritten by standard settings of
    // the CLI framework!
    // Therefore, the following defaults are only relevant
    // for development, testing and analysis runs.
    // CLI-override.
    activeReasoner = ActiveReasoner.Hermit,
    timeout = 60000,
    retry = 0,
    optimizeCandidates = true,
    renameToken = "٭",
    prefix = ":",
    log = true,
    debug = true,
    hidecolon = true,
    prettyVariableConcepts = true,
    printOutput = false,
    // No CLI-override (...yet)
    proxyFamily = true,
    arbitraryShapes = false
  )
