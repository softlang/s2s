package org.softlang.shass.infer

import org.softlang.shass.infer

import org.softlang.shass.query._
import de.pseifer.shar.dl._

trait Assumption(a: AtomicPatterns):
  def axioms: Set[Axiom]
