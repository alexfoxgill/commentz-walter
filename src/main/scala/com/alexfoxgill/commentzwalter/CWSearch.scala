package com.alexfoxgill.commentzwalter

import com.alexfoxgill.commentzwalter.Trie._

import scala.annotation.tailrec
import scala.collection.mutable

class CWSearchNode(
  val children: Map[Char, CWSearchNode],
  val terminal: Boolean,
  val shift1: Int,
  val shift2: Int,
  val depth: Int) {

  def next(c: Char) = children.get(c).orNull

}

case class Result(start: Int, text: String)

class CWSearch(minLen: Int, root: CWSearchNode, char: Char => Int) {

  def search(text: String): Option[Result] = {
    var i = minLen - 1

    while (i < text.length) {
      var current = root
      var candidate = current.next(text(i - current.depth))
      while (candidate != null) {
        val j = current.depth

        if (candidate.terminal) {
          return Some(Result(i - j, text.substring(i - j, i + 1)))
        }

        current = candidate
        candidate = current.next(text(i - current.depth))
      }

      i += Math.min(current.shift2, Math.max(current.shift1, char(text(1 + i - current.depth)) - current.depth - 1))
    }

    None
  }

}

object CWSearch {
  type Node = Trie[Char]

  @tailrec def isSuffix(a: Node, b: Node): Boolean = {
    (a, b) match {
      case (Vertex(aa, ap), Vertex(bb, bp)) => aa == bb && isSuffix(ap, bp)
      case (Root(), _) => true
      case _ => false
    }
  }

  def memoize[A, B](f: A => B): A => B = {
    val map = mutable.Map.empty[A, B]
    (n: A) => map.getOrElseUpdate(n, f(n))
  }

  class BuildState(root: Root[Char]) {

    val allNodes = root.descendantsAndSelf.toList

    val minDepth = allNodes.filter(_.isTerminal).map(_.depth).min

    val set1: Node => Set[Node] = memoize { n =>
      allNodes.filter(x => x != n && isSuffix(n, x)).toSet
    }

    val set2: Node => Set[Node] = memoize { n =>
      set1(n).filter(x => x.isTerminal)
    }

    val shift1: Node => Int = memoize {
      case Root() => 1
      case n@Vertex(_, _) =>
        set1(n)
          .map(_.depth - n.depth)
          .fold(minDepth)(Math.min)
    }

    val shift2: Node => Int = memoize {
      case Root() => minDepth
      case n@Vertex(_, p) =>
        set2(n)
          .map(_.depth - n.depth)
          .fold(shift2(p))(Math.min)
    }

    val convert: Node => CWSearchNode = memoize { n =>
      new CWSearchNode(
        n.childNodes
          .map(x => x.value -> convert(x))
          .toMap,
        n.isTerminal,
        shift1(n),
        shift2(n),
        n.depth)
    }

    val char: Char => Int = {
      val lookup = mutable.Map.empty[Char, Int]
      root.descendantsAndSelf
        .collect { case n@Vertex(v, _) => v -> n.depth }
        .foreach { case (c, d) =>
          lookup.get(c) match {
            case Some(dd) if d < dd => lookup(c) = d
            case None => lookup(c) = d
            case _ => ()
          }
        }
      c => lookup.getOrElse(c, minDepth + 1)
    }

    def result(): CWSearch =
      new CWSearch(minDepth, convert(root), char)

  }

  class Builder {
    private val root = new Root[Char]

    def add(word: String): Unit = {
      root.add(word.toList.reverse)
    }

    def build(): CWSearch = {
      new BuildState(root)
        .result()
    }
  }
}