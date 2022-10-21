package org.softlang.s2s.core

/** A set of configurations for S2S. */
case class Configuration(
    // *****************
    // *** Algorithm ***
    // *****************

    // In the DCA of pattern P, replace variables with T
    erasePvariables: Boolean = false,

    // In the DCA of tempalte H, replace variables with T
    eraseHvariables: Boolean = false,

    // In the DCA of pattern P, replace variables with an approximation.
    // Overriden by erasePvariables.
    approximatePvariables: Boolean = false,

    // In the DCA of tempalte H, replace variables with an approximation.
    // Overriden by eraseHvariables.
    approximateHvariables: Boolean = false,

    // Optimize candidate generation.
    optimizeCandidates: Boolean = true,

    // *************
    // *** Other ***
    // *************

    // Standard prefix (for examples).
    // Otherwise, supply prefix definitions in query.
    prefix: String = ":",

    // **************************
    // *** Logging and Output ***
    // **************************

    // Print log to stdout when calling Shapes2Shapes.run.
    log: Boolean = true,

    // Generate more detailed, debugging output.
    debug: Boolean = false,

    // More formal notation in output when using ':' as a prefix.
    hidecolon: Boolean = false,

    // Replace the shar: prefix (variable concepts) with '?'.
    prettyVariableConcepts: Boolean = false
):

  /** Left biased for non-boolean options, join for boolean options. */
  def join(cfg: Configuration): Configuration =
    Configuration(
      erasePvariables = this.erasePvariables || cfg.erasePvariables,
      eraseHvariables = this.eraseHvariables || cfg.eraseHvariables,
      approximatePvariables =
        this.approximatePvariables || cfg.approximatePvariables,
      approximateHvariables =
        this.approximateHvariables || cfg.approximateHvariables,
      optimizeCandidates = this.optimizeCandidates || cfg.optimizeCandidates,
      prefix = this.prefix, // Left-biased
      log = this.log || cfg.log,
      debug = this.debug || cfg.debug,
      hidecolon = this.hidecolon || cfg.hidecolon,
      prettyVariableConcepts =
        this.prettyVariableConcepts || cfg.prettyVariableConcepts
    )

/** Some preset configurations; can be combined using Configuratiion.join. */
object Configuration:

  private def djoin(left: Configuration, right: Configuration): Configuration =
    left.join(right)

  /** (Left-biased for non-boolean) join for multiple configurations. */
  def join(cfgs: Configuration*): Configuration =
    cfgs.foldLeft(default)(djoin)

  /** Default configuration. */
  def default: Configuration = Configuration()

  /** A configuration that only produces CWA/DCA/CWA. */
  def assumptionMethod: Configuration =
    Configuration(
      erasePvariables = false,
      eraseHvariables = false,
      approximatePvariables = false,
      approximateHvariables = false
    )

  /** A configuration that approximates variable concepts. */
  def philippsMethod: Configuration =
    Configuration(
      erasePvariables = false,
      eraseHvariables = false,
      approximatePvariables = true,
      approximateHvariables = true
    )

  /** A configuration that radically approximates variable concepts (via T). */
  def steffensMethod: Configuration =
    Configuration(
      erasePvariables = true,
      eraseHvariables = true,
      approximatePvariables = false,
      approximateHvariables = false
    )

  /** A configuration for more formal output. */
  def formalOutput: Configuration =
    Configuration(
      log = true,
      hidecolon = true,
      prefix = ":",
      prettyVariableConcepts = true
    )

  /** A configuration for debugging. */
  def debug: Configuration =
    Configuration(
      log = true,
      debug = true
    )
