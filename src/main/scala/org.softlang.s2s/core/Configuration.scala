package org.softlang.s2s.core

import scala.collection.BitSet
import java.io.ObjectInputFilter.Config

/** A set of configurations for S2S. */
case class Configuration(

    // *****************
    // *** Algorithm ***
    // *****************

    /** Generate DCA for pattern P. */
    dcaForPattern: Boolean = false,

    /** Generate DCA for template H. */
    dcaForTemplate: Boolean = false,

    /** Generate CWA for pattern P. */
    cwaForPattern: Boolean = false,

    /** Generate CWA for template H. */
    cwaForTemplate: Boolean = false,

    /** Generate UNA for pattern P. */
    unaForPattern: Boolean = false,

    /** Generate UNA for template H. */
    unaForTemplate: Boolean = false,

    // ***********
    // *** DCA ***
    // ***********

    /** In the DCA of pattern P, replace variables with T */
    erasePvariables: Boolean = false,

    /** In the DCA of tempalte H, replace variables with T */
    eraseHvariables: Boolean = false,

    /** In the DCA of pattern P, replace variables with an approximation.
        Overriden by erasePvariables. */
    approximatePvariables: Boolean = false,

    /** In the DCA of tempalte H, replace variables with an approximation.
        Overriden by eraseHvariables. */
    approximateHvariables: Boolean = false,

    // ***********
    // *** CWA ***
    // ***********

    /** Closure for concepts. */
    closeConcepts: Boolean = false,

    /** Closure for properties. */
    closeProperties: Boolean = false,

    /** Closure for T. */
    closeTop: Boolean = false,

    // *************
    // *** Other ***
    // *************

    /** Optimize candidate generation. */
    optimizeCandidates: Boolean = true,

    /** Standard prefix (for examples).
        Otherwise, supply prefix definitions in query. */
    prefix: String = ":",

    // **************************
    // *** Logging and Output ***
    // **************************

    /** Print log to stdout when calling Shapes2Shapes.run. */
    log: Boolean = false,

    /** Generate more detailed, debugging output. */
    debug: Boolean = false,

    /** More formal notation in output when using ':' as a prefix. */
    hidecolon: Boolean = false,

    /** Replace the shar: prefix (variable concepts) with '?'. */
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
      closeConcepts = this.closeConcepts || cfg.closeConcepts,
      closeProperties = this.closeProperties || cfg.closeProperties,
      closeTop = this.closeTop || cfg.closeTop,
      dcaForPattern = this.dcaForPattern || cfg.dcaForPattern,
      dcaForTemplate = this.dcaForTemplate || cfg.dcaForTemplate,
      cwaForPattern = this.cwaForPattern || cfg.cwaForPattern,
      cwaForTemplate = this.cwaForTemplate || cfg.cwaForTemplate,
      unaForPattern = this.unaForPattern || cfg.unaForPattern,
      unaForTemplate = this.unaForTemplate || cfg.unaForTemplate,
      optimizeCandidates = this.optimizeCandidates || cfg.optimizeCandidates,
      prefix = this.prefix, // Left-biased!
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
    cfgs.foldLeft(Configuration())(djoin)

  /** Default configuration for Shapes2Shapes. */
  def default: Configuration = Configuration(
    erasePvariables = false,
    eraseHvariables = false,
    approximatePvariables = false,
    approximateHvariables = false,
    closeConcepts = true,
    closeProperties = true,
    closeTop = false,
    dcaForPattern = true,
    dcaForTemplate = false,
    cwaForPattern = true,
    cwaForTemplate = true,
    unaForPattern = false,
    unaForTemplate = true,
    optimizeCandidates = true
  )

  /** A configuration adding formal output. */
  def formalOutput: Configuration =
    Configuration(
      log = true,
      hidecolon = true,
      prefix = ":",
      prettyVariableConcepts = true
    )

  /** A configuration adding debugging. */
  def debug: Configuration =
    Configuration(
      log = true,
      debug = true
    )

  /** Create from BitSet; unsafe, internal use only. */
  def fromBitset(bits: Int): Configuration = 
    def get(index: Int): Boolean = 
      if index >= bits.toBinaryString.size then false
      else if bits.toBinaryString(index) == '0' then false else true

    Configuration(
      dcaForPattern = get(0),
      dcaForTemplate = get(1),
      cwaForPattern = get(2),
      cwaForTemplate = get(3),
      unaForPattern = get(4),
      unaForTemplate = get(5),
      erasePvariables = get(6),
      eraseHvariables = get(7),
      approximatePvariables = get(8),
      approximateHvariables = get(9),
      closeConcepts = get(10),
      closeProperties = get(11),
      closeTop = get(12)
    )
