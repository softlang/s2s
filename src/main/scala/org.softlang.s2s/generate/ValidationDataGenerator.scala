package org.softlang.s2s.generate

import org.softlang.s2s.infer.AlgorithmInput
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.Log
import org.softlang.s2s.parser.JsonLDParser
import org.softlang.s2s.core.dropScope

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import de.pseifer.shar.core.BackendState
import scala.annotation.threadUnsafe

/** A generator for validation data, based on test samples (see
  * ValidationSuite.scala) or random generation (see Generator.scala).
  *
  * @param state
  *   BackendState of the applicable shar instance.
  */
class ValidationDataGenerator(state: BackendState):

  private val dataPath = System.getProperty("user.dir") ++ "/validation/data/"

  def generate(
      input: AlgorithmInput,
      output: Set[SHACLShape],
      name: String,
      log: Log,
      gen: Boolean = false
  ): Unit =

    // Produce all validation data.
    val query = input.formatQuery(state)
    val sin = input.formatShapes.toOption.get
    val sout = JsonLDParser
      .unparse(output.map(_.dropScope(input.getScopes)))
      .toOption
      .get

    val shardikKB = log.format(false, false, false, true)

    val cvoc = input.vocabularyIn.concepts
      .map(_.dropScope(input.getScopes))
      .mkString("\n")
      .filterNot(c => c == '<' || c == '>')
    val pvoc = input.vocabularyIn.properties
      .map(_.dropScope(input.getScopes))
      .mkString("\n")
      .filterNot(c => c == '<' || c == '>')
    val ivoc = input.vocabularyIn.nominals
      .map(_.dropScope(input.getScopes))
      .mkString("\n")
      .filterNot(c => c == '<' || c == '>')

    val (subdir, qname) =
      if gen then
        if input.isECCQ
        then ("generated/", "query.gcore")
        else ("generated/", "query.sparql")
      else if input.isECCQ
      then ("eccq/", "query.gcore")
      else ("sccq/", "query.sparql")

    // The base path of the 'validation' directory.
    val base = dataPath ++ subdir ++ SampleID.next(name)

    // Query.
    val qfile = Paths.get(base ++ "/" ++ qname)
    Files.createDirectories(qfile.getParent())
    Files.write(qfile, query.getBytes(StandardCharsets.UTF_8))

    // Input shapes.
    val isfile = Paths.get(base ++ "/in.json")
    Files.write(isfile, sin.getBytes(StandardCharsets.UTF_8))

    // Output shapes.
    val osfile = Paths.get(base ++ "/out.json")
    Files.write(osfile, sout.getBytes(StandardCharsets.UTF_8))

    // Vocabularies.
    val cvfile = Paths.get(base ++ "/concepts.vocabulary")
    Files.write(cvfile, cvoc.getBytes(StandardCharsets.UTF_8))

    val pvfile = Paths.get(base ++ "/properties.vocabulary")
    Files.write(pvfile, pvoc.getBytes(StandardCharsets.UTF_8))

    val ivfile = Paths.get(base ++ "/nominals.vocabulary")
    Files.write(ivfile, ivoc.getBytes(StandardCharsets.UTF_8))

    // Executable shardik KB.
    val kbfile = Paths.get(base ++ "/shardik.kb")
    Files.write(kbfile, shardikKB.getBytes(StandardCharsets.UTF_8))

  private object SampleID:
    private var count: Map[String, Int] = Map()

    /** Make a unique name from actual test name and a running ID. */
    def next(name: String): String =
      val c = count.getOrElse(name, 0)
      count = count + (name -> (c + 1))
      if c == 0 then s"${name}"
      else s"${name}_${c}"
