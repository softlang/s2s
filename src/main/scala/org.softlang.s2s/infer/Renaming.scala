package org.softlang.s2s.infer

import de.pseifer.shar.dl.Axiom
import org.softlang.s2s.query.AtomicPatterns
import de.pseifer.shar.dl._
import org.softlang.s2s.core.rename

trait Renaming(
    conceptRenaming: Boolean,
    propertyRenaming: Boolean,
    renameToken: String
):

  /** Rename a named concept, if enabled. */
  protected def rename(nc: NamedConcept): NamedConcept =
    if conceptRenaming then NamedConcept(nc.c.rename(renameToken)) else nc

  /** Rename a named role, if enabled. */
  protected def rename(nr: NamedRole): NamedRole =
    if propertyRenaming then NamedRole(nr.r.rename(renameToken)) else nr
