package org.softlang.s2s

import org.softlang.s2s.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.query.SCCQ

import scala.util.Random

class ConfigurationComparison(
    c1: Configuration, c2: Configuration,
    stepTrials: Int = 100, randomSets: Int = 10):

    // Test Instances.
    val s1 = Shapes2Shapes(c1)
    val s2 = Shapes2Shapes(c2)

    // Test `n` trials to identify a difference.
    private def search(n: Int, qg: QueryGenerator, sg: ShapeGenerator)
            : Either[(SCCQ, Set[SimpleSHACLShape]), Int] 
        = 
            def doSearch(trial: Int): Either[(SCCQ, Set[SimpleSHACLShape]), Int] =
                print(".")
                if trial <= 0 then 
                    Right(n)
                else
                    val q = qg.draw
                    val s = sg.draw

                    val log1 = Log(debugging = true)
                    val log2 = Log(debugging = true)

                    val r1 = s1.algorithm(q, s, log1)
                    val r2 = s2.algorithm(q, s, log2)

                    if r1 != r2 then
                        println()
                        log1.print(true, true)
                        log2.print(true, true)
                        Left(q,s)
                    else doSearch(trial - 1)
            doSearch(n)

    // Run trials with this configuration.
    private def step(
        minNumberOfVariables: Int,
        minNumberOfConcepts: Int,
        minNumberOfProperties: Int,
        minNumberOfNominals: Int,
        minPatterns: Int, 
        maxPatterns: Int, 
        shapes: Int,
        trials: Int = stepTrials): Either[(SCCQ, Set[SimpleSHACLShape]), Int] =
            print(List(minNumberOfVariables,
                       minNumberOfConcepts,
                       minNumberOfProperties,
                       minNumberOfNominals,
                       minPatterns,
                       maxPatterns,
                       shapes).mkString("."))
            val voc = VocabularyGenerator(
                minNumberOfVariables,
                minNumberOfVariables + 1,
                minNumberOfConcepts,
                minNumberOfConcepts + 1,
                minNumberOfProperties,
                minNumberOfProperties + 1,
                minNumberOfNominals,
                minNumberOfNominals + 1).draw
            search(trials, 
                QueryGenerator(voc, minPatterns, maxPatterns), 
                ShapeGenerator(voc, shapes, true))

    // Generate set sets of random trials (with stepTrials each).
    private def random(set: Int): Either[(SCCQ, Set[SimpleSHACLShape]), Int] =
        if set <= 0 then
            Right(0)
        else
            for 
                r <- random(set - 1)
                p1 = Random.between(1,3)
                p2 = Random.between(1,3)
                n <- step(
                    Random.between(1,3),
                    Random.between(1,3),
                    Random.between(1,3),
                    Random.between(0,1), // TODO
                    p1.min(p2),
                    p1.max(p2) + 1,
                    Random.between(1,3))
            yield n + r

    /** Run randomSets * stepTrials random trials. */
    def randomized: Unit =
        for r <- random(randomSets)
        do println("\nExecuted " ++ r.toString ++ " trials without differences.")

    /** Widen search space in 10 steps * stepTrials, 
     *  then finish with randomSets * stepTrials random trials. */
    def structured: Unit = 

        // Widen search space with each step.
        for
            // Only patterns x : A
            n0 <- step(1,2,0,0,2,4,0)
            n1 <- step(1,2,0,0,2,4,1)
            n2 <- step(1,2,0,0,2,4,2)
            n3 <- step(2,2,0,0,2,4,1)
            n4 <- step(2,2,0,0,2,4,2)

            // Arbitrary patterns
            n5 <- step(1,2,2,0,2,4,0)
            n6 <- step(1,2,2,0,2,4,1)
            n7 <- step(1,2,2,0,2,4,2)
            n8 <- step(2,2,2,0,2,4,1)
            n9 <- step(2,2,2,0,2,4,2)

            // Random trials
            r <- random(randomSets)
        do 
            println("\nExecuted " 
                ++ (n0 + n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8 + n9 + r).toString 
                ++ " trials without differences.")