package com.alexfoxgill.commentzwalter

import utest._

object CWSearchSpec extends TestSuite {
  def fixture(keywords: String*) = {
    val builder = new CWSearch.Builder()
    keywords.foreach(builder.add)
    builder.build()
  }

  val tests = Tests {
    test("returns None if no string found") {
      val search = fixture("a")

      search.search("") ==> None
    }

    test("finds a keyword in itself") {
      val search = fixture("a")

      search.search("a") ==> Some(Result(0, "a"))
    }

    test("finds a keyword at the start") {
      val search = fixture("a")

      search.search("ab") ==> Some(Result(0, "a"))
    }

    test("finds a keyword at the end") {
      val search = fixture("b")

      search.search("ab") ==> Some(Result(1, "b"))
    }

    test("finds the first keyword in the string") {
      val search = fixture("a", "b")

      search.search("ab") ==> Some(Result(0, "a"))
    }

    test("finds a keyword among similar") {
      val search = fixture("aa", "ab")

      search.search("baa") ==> Some(Result(1, "aa"))
    }

    test("works when going down an incorrect path initially") {
      val search = fixture("bb")

      search.search("abb") ==> Some(Result(1, "bb"))
    }
  }
}
