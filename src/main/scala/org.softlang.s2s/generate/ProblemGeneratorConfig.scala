package org.softlang.s2s.generate

/** A configuration for a generator. */
case class ProblemGeneratorConfig(
    // Query parameters.

    // Use a query file as input, instead of a generator.
    inputFile: Option[String],

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
    // Number of additional shapes (not only from the vocabulary).
    // Ratio of property-based vs. Concept targets.
    // Set to -1.0 for default.
    propertyConceptTargetRatio: FloatParameter,
    // Ratio of property (exists, forall) vs. Concept constraints.
    // Set to -1.0 for default.
    propertyConceptConstraintRatio: FloatParameter,
    // Allow universal quantification in constraints.
    includeForallConstraints: Boolean,

    // Random seed. Use "" for random seed.
    seed: String = ""
):

  private def fieldNames: List[String] =
    List(
      "InputFile",
      "MinPatternSize",
      "MaxPatternSize",
      "MinTemplateSize",
      "MaxTemplateSize",
      "FreshVariable",
      "VariablesCount",
      "FreshConcept",
      "ConceptsCount",
      "FreshProperty",
      "PropertiesCount",
      "FreshNominal",
      "NominalsCount",
      "PropertyConceptRatio",
      "VariableToNominalRatio",
      "CyclicRedrawCount",
      "MinNumberOfShapes",
      "MaxNumberOfShapes",
      "PropertyConceptTargetRatio",
      "PropertyConceptConstraintRatio",
      "IncludeForallConstraints",
      "Seed"
    )

  private def fields: List[String] =
    List(
      inputFile,
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
      cyclicRedrawCount,
      minNumberOfShapes,
      maxNumberOfShapes,
      propertyConceptTargetRatio,
      propertyConceptConstraintRatio,
      includeForallConstraints,
      seed
    ).map(_.toString)

  override def toString: String = fields.mkString("_")

  def formatLong: String =
    fieldNames.zip(fields).map((n, f) => s"$n: $f").mkString("\n")
