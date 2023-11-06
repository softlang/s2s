package org.softlang.s2s.test

import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes

class ScopesTests extends munit.FunSuite:

  //  Scope tokens.

  test("getToken Scope.None") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.None), "")
  }

  test("getToken Scope.In") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.In), "abc0")
  }

  test("getToken Scope.Med") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.Med), "abc1")
  }

  test("getToken Scope.Out") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.Out), "abc2")
  }

  test("getToken Scope.Variable") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.Variable), "abc-1")
  }

  test("getToken (empty)") {
    val scopes = Scopes("", 0, 1, 2, -1)
    assertEquals(scopes.getToken(Scope.None), "")
    assertEquals(scopes.getToken(Scope.In), "0")
    assertEquals(scopes.getToken(Scope.Med), "1")
    assertEquals(scopes.getToken(Scope.Out), "2")
    assertEquals(scopes.getToken(Scope.Variable), "-1")
  }

  // Token removal.
  
  test("tokenRemoval Scope.In") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc0"), "hello")
  }

  test("tokenRemoval Scope.Med") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc1"), "hello")
  }

  test("tokenRemoval Scope.Out") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc2"), "hello")
  }

  test("tokenRemoval Scope.Variable") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc-1"), "hello")
  }

  test("tokenRemoval (none) 1") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("helloabc"), "helloabc")
  }

  test("tokenRemoval (none) 2") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("just1randomword"), "just1randomword")
  }

  test("tokenRemoval (none) 3") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("0"), "0")
  }

  test("tokenRemoval (none) 4") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens("abcd0"), "abcd0")
  }

  test("tokenRemoval (empty)") {
    val scopes = Scopes("abc", 0, 1, 2, -1)
    assertEquals(scopes.removeScopeTokens(""), "")
  }

