package org.softlang.s2s.test

import org.softlang.s2s.generate.ThingGenerator
import scala.util.Random

class ThingGeneratorTests extends munit.FunSuite:

  // Note: Some of these tests might be flaky due to randomness.
  // This indicates a bug; most test are designed for
  // random success to be unlikely.

  test("Size one generates one value") {
    val gen = ThingGenerator[Int](0.5, 1, id => id, Random())
    val t1 = gen.sample()
    val t2 = gen.sample()
    val l = List.fill(1000)(gen.sample())
    assertEquals(l.sum, 1000)
  }

  test("Selection choose existing samples") {
    val gen = ThingGenerator[Int](0.5, 10, id => id, Random())
    val r1 = gen.fresh()
    val r2 = gen.fresh()
    val l = List.fill(1000)(gen.select())
    assert(l.sum >= 1000)
    assert(l.sum <= 2000)
    assert(l.forall(i => i == 1 || i == 2))
  }

  test("Max is obeyed") {
    val gen = ThingGenerator[Int](0.5, 10, id => id, Random())
    // Generated values == index here, so max should be '10'.
    val l = List.fill(1000)(gen.sample())
    assertEquals(l.max, 10)

    val gen2 = ThingGenerator[Int](0.5, 0, id => id, Random())
    // Without upper limit and using fresh every time, should be 1000.
    val l2 = List.fill(1000)(gen2.fresh())
    assertEquals(l2.max, 1000)
    assertEquals(l2, l2.sorted)
  }

  test("Locking works") {
    val gen = ThingGenerator[Int](0.5, 0, id => id, Random())
    gen.lock()
    val l = List.fill(1000)(gen.sample())
    assertEquals(l, l.sorted)

    gen.unlock()
    gen.fresh()
    gen.lock()

    val l2 = List.fill(1000)(gen.sample())
    assertEquals(l2.max, 2)
    gen.unlock()
  }
