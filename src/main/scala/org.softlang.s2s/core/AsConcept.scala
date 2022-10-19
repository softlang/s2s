package org.softlang.s2s.core

import de.pseifer.shar.dl.Concept

/** Convertable to a DL Concept.
  */
trait AsConcept:

  /** Convert to a DL concept. */
  def asConcept: Concept
