package org.softlang.s2s.core

import java.io.ObjectInputFilter.Config
import scala.collection.BitSet

/** A set of configurations for S2S. */
case class Configuration(
    // *****************
    // *** Algorithm ***
    // *****************

    // Generate DCA for pattern P.
    dcaForPattern: Boolean = false,

    // Generate DCA for template H.
    dcaForTemplate: Boolean = false,

    // Generate CWA for pattern P.
    cwaForPattern: Boolean = false,

    // Generate CWA for template H.
    cwaForTemplate: Boolean = false,

    // Generate UNA for pattern P.
    unaForPattern: Boolean = false,

    // Generate UNA for template H.
    unaForTemplate: Boolean = false,

    // Use the mapping method.
    useMappingMethod: Boolean = false, //                 TODO - NOT IMPLEMENTED

    // Rename internal pattern concepts.
    renamePatternInternal: Boolean = false,

    // ***********
    // *** DCA ***
    // ***********

    // In the DCA of pattern P, replace variables with T.
    erasePvariables: Boolean = false,

    // In the DCA of tempalte H, replace variables with T.
    eraseHvariables: Boolean = false,

    // In the DCA of pattern P, replace variables with an approximation.
    // Overriden by erasePvariables.
    approximatePvariables: Boolean = false,

    // In the DCA of tempalte H, replace variables with an approximation.
    // Overriden by eraseHvariables.
    approximateHvariables: Boolean = false,

    // Use subsumption instead of equality for the pattern.
    useSubsumptionInPatternDCA: Boolean = false,

    // Use subsumption instead of equality for the template.
    useSubsumptionInTemplateDCA: Boolean = false,

    // ***********
    // *** CWA ***
    // ***********

    // Closure for concepts.
    closeConcepts: Boolean = false,

    // Closure for properties.
    closeProperties: Boolean = false,

    // Closure for T.
    closeTop: Boolean = false,

    // Closure for literals {a}.
    closeLiterals: Boolean = false,

    // Use subsumption instead of equality for the pattern.
    useSubsumptionInPatternCWA: Boolean = false,

    // Use subsumption instead of equality for the template.
    useSubsumptionInTemplateCWA: Boolean = false,

    // ********************
    // *** User Options ***
    // ********************

    // Optimize candidate generation.
    optimizeCandidates: Boolean = false,

    // Automatically rename input concepts and properties.
    autoRename: Boolean = false,

    // Token to append (if autoRename is set).
    renameToken: String = "'",

    // Standard predefined prefix.
    prefix: String = ":",

    // Print log to stdout when calling Shapes2Shapes.run.
    log: Boolean = false,

    // Generate more detailed, debugging output.
    debug: Boolean = false,

    // More formal notation in output when using ':' as a prefix.
    hidecolon: Boolean = false,

    // Replace the shar: prefix (variable concepts) with '?'.
    prettyVariableConcepts: Boolean = false,

    // Print output shapes (outside of log) to output.
    printOutput: Boolean = false
)

/** Some preset configurations. */
object Configuration:

  /** Default configuration for Shapes2Shapes. */
  def default: Configuration = Configuration(
    // Algorithm
    dcaForPattern = true,
    dcaForTemplate = true,
    cwaForPattern = true,
    cwaForTemplate = true,
    unaForPattern = false,
    unaForTemplate = true,
    useMappingMethod = false,
    renamePatternInternal = true,
    // DCA
    erasePvariables = false,
    eraseHvariables = false,
    approximatePvariables = false,
    approximateHvariables = false,
    useSubsumptionInPatternDCA = false,
    useSubsumptionInTemplateDCA = true,
    // CWA
    closeConcepts = true,
    closeProperties = true,
    closeTop = false,
    closeLiterals = false,
    useSubsumptionInPatternCWA = false,
    useSubsumptionInTemplateCWA = false,
    // The following can be set by users via CLI
    // and are overwritten by standard settings of
    // the CLI framework!
    // Therefore, the following defaults are only relevant
    // for development work, in particular, testing.
    optimizeCandidates = true,
    autoRename = false,
    renameToken = "'",
    prefix = ":",
    log = true,
    debug = true,
    hidecolon = true,
    prettyVariableConcepts = true,
    printOutput = false
  )
