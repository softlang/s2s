package org.softlang.s2s.core

/** A set of configurations for S2S. */
case class Configuration(
    // *****************
    // *** Algorithm ***
    // *****************

    // Generate DCA for pattern P.
    dcaForPattern: Boolean,

    // Generate DCA for template H.
    dcaForTemplate: Boolean,

    // Generate CWA for pattern P.
    cwaForPattern: Boolean,

    // Generate CWA for template H.
    cwaForTemplate: Boolean,

    // Generate UNA for pattern P.
    unaForPattern: Boolean,

    // Generate UNA for template H.
    unaForTemplate: Boolean,

    // Use the mapping method.
    useMappingMethod: Boolean,

    // Generate property subsumption axioms.
    addPropertySubsumptions: Boolean,

    // Rename internal pattern concepts.
    renamePatternInternalConcepts: Boolean,

    // Rename internal pattern Properties.
    renamePatternInternalProperties: Boolean,

    // Use namespace-specific concepts for T.
    useNamespacedTop: Boolean,

    // Name for the namespaced T.
    namespacedTopName: String,

    // ***********
    // *** DCA ***
    // ***********

    // In the DCA of pattern P, replace variables with T.
    erasePvariables: Boolean,

    // In the DCA of tempalte H, replace variables with T.
    eraseHvariables: Boolean,

    // In the DCA of pattern P, replace variables with an approximation.
    // Overriden by erasePvariables.
    approximatePvariables: Boolean,

    // In the DCA of tempalte H, replace variables with an approximation.
    // Overriden by eraseHvariables.
    approximateHvariables: Boolean,

    // Use subsumption instead of equality for the pattern.
    useSubsumptionInPatternDCA: Boolean,

    // Use subsumption instead of equality for the template.
    useSubsumptionInTemplateDCA: Boolean,

    // ***********
    // *** CWA ***
    // ***********

    // Closure for concepts.
    closeConcepts: Boolean,

    // Closure for properties.
    closeProperties: Boolean,

    // Closure for literals {a}.
    closeLiterals: Boolean,

    // Use subsumption instead of equality for the pattern.
    useSubsumptionInPatternCWA: Boolean,

    // Use subsumption instead of equality for the template.
    useSubsumptionInTemplateCWA: Boolean,

    // ********************
    // *** User Options ***
    // ********************

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

  /** Default configuration for Shapes2Shapes. */
  def default: Configuration = Configuration(
    // Algorithm
    dcaForPattern = true,
    dcaForTemplate = true,
    cwaForPattern = false,
    cwaForTemplate = true,
    unaForPattern = false,
    unaForTemplate = true,
    useMappingMethod = true,
    addPropertySubsumptions = true,
    renamePatternInternalConcepts = true,
    renamePatternInternalProperties = true,
    useNamespacedTop = true,
    namespacedTopName = "T",
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
    closeLiterals = false,
    useSubsumptionInPatternCWA = false,
    useSubsumptionInTemplateCWA = false,
    // The following can be set by users via CLI
    // and are overwritten by standard settings of
    // the CLI framework!
    // Therefore, the following defaults are only relevant
    // for development work, in particular, testing.
    optimizeCandidates = true,
    renameToken = "٭",
    prefix = ":",
    log = true,
    debug = true,
    hidecolon = true,
    prettyVariableConcepts = true,
    printOutput = false
  )
