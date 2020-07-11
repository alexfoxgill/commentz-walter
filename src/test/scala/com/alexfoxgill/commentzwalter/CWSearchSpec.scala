package com.alexfoxgill.commentzwalter

import utest._

object CWSearchSpec extends TestSuite {
  def fixture(keywords: String*) = {
    val builder = new CWSearch.Builder()
    keywords.foreach(builder.add)
    builder.build()
  }

  val tests = Tests {
    test("it returns None if no string found") {
      val search = fixture("test")

      search.search("") ==> None
    }

    test("it finds a keyword in itself") {
      val search = fixture("test")

      search.search("test") ==> Some(Result(0, "test"))
    }

    test("it finds a keyword at the start") {
      val search = fixture("test")

      search.search("test1") ==> Some(Result(0, "test"))
    }

    test("it finds a keyword at the end") {
      val search = fixture("test")

      search.search("1test") ==> Some(Result(1, "test"))
    }

    test("it finds the first keyword in the string") {
      val search = fixture("foo", "bar")

      search.search("foobar") ==> Some(Result(0, "foo"))
    }
  }
}
