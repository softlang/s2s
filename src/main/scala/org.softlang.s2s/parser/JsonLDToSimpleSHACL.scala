package org.softlang.s2s.parser

import java.io.FileReader
import com.apicatalog.jsonld.document.JsonDocument
import com.apicatalog.jsonld.JsonLd

class JsonLDToSimpleShacl(file: String):

  val jDoc = JsonDocument.of(FileReader(file))
  val ld = JsonLd.expand(jDoc).get()

  // TODO

  def convert: Set[String] =
    throw new RuntimeException("JSON-LD is currently not supported.")
    Set()
