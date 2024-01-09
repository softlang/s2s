package org.softlang.s2s.test

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Prefix
// import de.pseifer.shar.core.Iri
// import de.pseifer.shar.dl._

import org.softlang.s2s.core._
// import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.parser.ShapeParser
import org.softlang.s2s.parser.JsonLDParser
// import org.softlang.s2s.query.GCORE
// import org.softlang.s2s.query.SCCQ
// import org.softlang.s2s.query.AtomicPattern

class JsonLDtoDLTests extends munit.FunSuite:
 
  // Utility: Parse SHACL shapes from concept notation.

  val shar = Shar()
  import shar._
  for
    p <- Prefix.fromString(Configuration.default.prefix)
    i <- Iri.fromString("<https://github.com/softlang/s2s/>")
  do shar.state.prefixes.add(p, i)

  private val shapeParser = ShapeParser(shar)

  def parseFormal(formal: String): Set[SHACLShape] =
    val sp = for s <- Util
        .flipEitherHead(formal
          .linesIterator
          .map(_.trim)
          .filter(_.nonEmpty) 
          .map(shapeParser.parseGeneral(_))
          .toList)
        .map(_.toSet)
    yield s.toList.toSet
    assert(sp.isRight, "error in test case (can not parse control)")
    sp.toOption.get

  // Utility: Parse jsonLD
  
  def parseJson(shapes: String): Set[SHACLShape] = 
    val ps = JsonLDParser.fromString(shapes)
    val sp = for s <- Util
        .flipEitherHead(ps.map(JsonLDParser.parse(_)).toList)
        .map(_.toSet)
    yield s.toList.toSet
    assert(sp.isRight, "unable to parse test case")
    sp.toOption.get

  // Work

  def work(json: String, formal: String): Unit = 
    val s1 = parseJson(json)
    val s2 = parseFormal(formal)
    assertEquals(s1, s2)

  // Test cases.

  test("class target") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  }
}
      """, 
      ":Person ⊑ ⊤")

    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@graph": [
    {
      "@id": "s2s:PersonShape",
      "@type": "sh:NodeShape",
      "sh:targetClass": {
        "@id": "s2s:Person"
      }
    },
    {
      "@id": "s2s:DogShape",
      "@type": "sh:NodeShape",
      "sh:targetClass": {
        "@id": "s2s:Dog"
      }
    }
  ]
}
      """, 
      """
      :Person ⊑ ⊤
      :Dog ⊑ ⊤
      """)
  }
   
  test("object target") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetObjectsOf": {
    "@id": "s2s:knows"
  }
}
      """, 
      "∃:knows.⊤ ⊑ ⊤")

    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@graph": [
    {
      "@id": "s2s:PersonShape",
      "@type": "sh:NodeShape",
      "sh:targetObjectsOf": {
        "@id": "s2s:knows"
      }
    },
    {
      "@id": "s2s:DogShape",
      "@type": "sh:NodeShape",
      "sh:targetObjectsOf": {
        "@id": "s2s:likes"
      }
    }
  ]
}
      """, 
      """
      ∃:knows.⊤ ⊑ ⊤
      ∃:likes.⊤ ⊑ ⊤
      """)
  }
    
  test("subject target") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetSubjectsOf": {
    "@id": "s2s:knows"
  }
}
      """, 
      "∃-:knows.⊤ ⊑ ⊤")

    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@graph": [
    {
      "@id": "s2s:PersonShape",
      "@type": "sh:NodeShape",
      "sh:targetSubjectsOf": {
        "@id": "s2s:knows"
      }
    },
    {
      "@id": "s2s:DogShape",
      "@type": "sh:NodeShape",
      "sh:targetSubjectsOf": {
        "@id": "s2s:likes"
      }
    }
  ]
}
      """, 
      """
      ∃-:knows.⊤ ⊑ ⊤
      ∃-:likes.⊤ ⊑ ⊤
      """)
  }

  test("class constraint") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:class": {
    "@id": "s2s:Agent"
  }
}
      """, 
      ":Person ⊑ :Agent")

    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:class": [
    {
      "@id": "s2s:Agent"
    },
    {
      "@id": "s2s:Dog"
    }
  ]
}
      """, 
      ":Person ⊑ :Agent ⊓ :Dog")
  }

  test("universal quantification") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:property": {
    "sh:path": {
      "@id": "s2s:knows"
    },
    "sh:class": {
      "@id": "s2s:Person"
    }
  }
}
      """, 
      ":Person ⊑ ∀:knows.:Person")
  }

  test("existential quantification") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:property": {
    "sh:path": {
      "@id": "s2s:knows"
    },
    "sh:qualifiedValueShape": {
      "sh:class": {
        "@id": "s2s:Person"
      }
    },
    "sh:qualifiedMinCount": 1
  }
}
      """, 
      ":Person ⊑ ∃:knows.:Person")
  }

  test("and") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:and": {
    "@list": [
      {
        "sh:class": {
          "@id": "s2s:Agent"
        }
      },
      {
        "sh:class": {
          "@id": "s2s:Dog"
        }
      }
    ]
  }
}
      """, 
      ":Person ⊑ :Agent ⊓ :Dog")
  }
  
  test("or") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:or": {
    "@list": [
      {
        "sh:class": {
          "@id": "s2s:Agent"
        }
      },
      {
        "sh:class": {
          "@id": "s2s:Dog"
        }
      }
    ]
  }
}
      """, 
      ":Person ⊑ :Agent ⊔ :Dog")
  }
  
  test("not") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:PersonShape",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:Person"
  },
  "sh:not": {
    "sh:class": {
      "@id": "s2s:Dog"
    }
  }
}
      """, 
      ":Person ⊑ ¬:Dog")
  }

  test("example shapes") {
    work(
      """
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@id": "s2s:s1",
  "@type": "sh:NodeShape",
  "sh:targetClass": {
    "@id": "s2s:A"
  },
  "sh:class": {
    "@id": "s2s:B"
  }
}
      """, 
      ":A ⊑ :B")
  }

