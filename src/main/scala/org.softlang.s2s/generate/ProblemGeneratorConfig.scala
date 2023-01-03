package org.softlang.s2s.generate

/** A configuration for a generator. */
case class ProblemGeneratorConfig(
    // Query parameters.

    // Min/max count of atomic patterns in Pattern.
    minPatternSize: IntParameter,
    maxPatternSize: IntParameter,
    // Min/max count of atomic patterns in Template.
    minTemplateSize: IntParameter,
    maxTemplateSize: IntParameter,
    // Probability of generating a fresh variable (0.0 to 1.0).
    freshVariable: FloatParameter,
    // Maximum number of variables.
    variablesCount: IntParameter,
    // Probability of generating a fresh concept (0.0 to 1.0).
    freshConcept: FloatParameter,
    // Total number of concepts allowed. 0 for unlimited.
    conceptsCount: IntParameter,
    // Probability of generating a fresh property (0.0 to 1.0).
    freshProperty: FloatParameter,
    // Total number of properties allowed. 0 for unlimited.
    propertiesCount: IntParameter,
    // Probability of generating a fresh nominal (0.0 to 1.0).
    freshNominal: FloatParameter,
    // Total number of nominals allowed. 0 for unlimited.
    nominalsCount: IntParameter,
    // Ratio of property patterns to concept patterns (0.0 to 1.0).
    propertyConceptRatio: FloatParameter,
    // Ratio of variables to nominals in patterns (0.0 to 1.0).
    variableToNominalRatio: FloatParameter,
    // Avoid self-circles by redrawing N times.
    cyclicRedrawCount: IntParameter,

    // Shape parameters.

    // Min/max number of input shapes.
    minNumberOfShapes: IntParameter,
    maxNumberOfShapes: IntParameter,
    // Ratio of property-based vs. Concept targets.
    // Set to -1.0 for default.
    propertyConceptTargetRatio: FloatParameter,
    // Ratio of property (exists, forall) vs. Concept constraints.
    // Set to -1.0 for default.
    propertyConceptConstraintRatio: FloatParameter,
    // Allow universal quantification in constraints.
    includeForallConstraints: Boolean
):

  override def toString: String =
    List(
      minPatternSize,
      maxPatternSize,
      minTemplateSize,
      maxTemplateSize,
      freshVariable,
      variablesCount,
      freshConcept,
      conceptsCount,
      freshProperty,
      propertiesCount,
      freshNominal,
      nominalsCount,
      propertyConceptRatio,
      variableToNominalRatio,
      minNumberOfShapes,
      maxNumberOfShapes,
      propertyConceptTargetRatio,
      propertyConceptConstraintRatio,
      includeForallConstraints
    ).mkString("_")
