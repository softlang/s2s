package org.softlang.s2s.main

import org.softlang.s2s.core.Configuration
import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.parser.JsonLDToSimpleShacl

import scala.util.Failure
import scala.util.Try

/** Shapes2Shapes application entry point. */
object S2s:
  def run(args: Seq[String]): Unit =
    // Initialize CLI configuration.
    val conf = CLIConfiguration(Configuration.default, args)

    // Create buffered sources and check for errors.
    val qft = Try(io.Source.fromFile(conf.queryFile()).getLines.mkString("\n"))
    val sft = Try(
      if conf.shapesFile().isEmpty then Set()
      else if conf.shapesFile().contains(".json") then
        JsonLDToSimpleShacl(conf.shapesFile()).convert
      else
        io.Source.fromFile(conf.shapesFile()).getLines.filter(_.nonEmpty).toSet
    )

    // Output (system) errors, if files can not be opened.
    def handle[T](t: Try[T]): Unit =
      t match
        case Failure(e) => println("FAILURE " + e.getLocalizedMessage)
        case _          => ()

    handle(qft)
    handle(sft)

    // Run Shapes2Shapes with default config and input query/shapes.
    for
      q <- qft
      s <- sft
    do Shapes2Shapes(conf.toConfiguration).run(q, s)
