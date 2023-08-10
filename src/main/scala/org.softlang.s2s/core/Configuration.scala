package org.softlang.s2s.core

/** A set of configurations for S2S. */
case class Configuration(
    // Set the reasoner to be used.
    reasoner: ActiveReasoner,

    // Timeout for reasoning attempts (in milliseconds).
    timeout: Long,

    // Times the reasoning is allowed to restart after timeout.
    retry: Int,

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
    printOutput: Boolean,

    // Settings for candidate generation.
    shapeHeuristic: ShapeHeuristic
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
    reasoner = ActiveReasoner.Hermit,
    timeout = 60000,
    retry = 0,
    renameToken = "٭",
    prefix = ":",
    log = true,
    debug = true,
    hidecolon = true,
    prettyVariableConcepts = true,
    printOutput = false,
    // No (complete) CLI-override (...yet)
    shapeHeuristic = ShapeHeuristic.default
  )
