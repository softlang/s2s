package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes

class ScopesTests:

  //  Scope tokens.

  @Test def testGetTokenNone(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.None), "")

  @Test def testGetTokenIn(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.In), "abc0")

  @Test def testGetTokenMed(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.Med), "abc1")

  @Test def testGetTokenOut(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.Out), "abc2")

  @Test def testGetTokenVariable(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.Variable), "abc-1")

  @Test def testGetTokenEmpty(): Unit =
    val scopes = Scopes("", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.None), "")
    assertEquals(scopes.getToken(Scope.In), "0")
    assertEquals(scopes.getToken(Scope.Med), "1")
    assertEquals(scopes.getToken(Scope.Out), "2")
    assertEquals(scopes.getToken(Scope.Variable), "-1")

  // Token removal.
  
  @Test def tokenRemovalIn(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc0"), "hello")

  @Test def tokenRemovalMed(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc1"), "hello")

  @Test def tokenRemovalOut(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc2"), "hello")

  @Test def tokenRemovalVariable(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc-1"), "hello")

  @Test def tokenRemovalNeg1(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc"), "helloabc")

  @Test def tokenRemovalNeg2(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("just1randomword"), "just1randomword")

  @Test def tokenRemovalNeg3(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("0"), "0")

  @Test def tokenRemovalNeg4(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("abcd0"), "abcd0")

  @Test def tokenRemoval(): Unit =
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens(""), "")
