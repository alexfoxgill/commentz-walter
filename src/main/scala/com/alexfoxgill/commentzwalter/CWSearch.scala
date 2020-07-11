package com.alexfoxgill.commentzwalter

import com.alexfoxgill.commentzwalter.Trie._

import scala.annotation.tailrec
import scala.collection.mutable

class CWSearchNode(
  val children: Map[Char, CWSearchNode],
  val terminal: Boolean,
  val shift1: Int,
  val shift2: Int,
  val depth: Int,
  getSet1: () => Set[CWSearchNode],
  getSet2: () => Set[CWSearchNode],
  getParent: () => Option[CWSearchNode]) {

  lazy val set1: Set[CWSearchNode] = getSet1()
  lazy val set2: Set[CWSearchNode] = getSet2()
  lazy val parent: Option[CWSearchNode] = getParent()

  def has(c: Char) = children.contains(c)
}

case class Result(start: Int, text: String)

class CWSearch(minLen: Int, root: CWSearchNode, char: Char => Int) {
  private def getChild(node: CWSearchNode, text: String, index: Int): CWSearchNode = {
    if (text.length < index) {
      return null
    }
    node.children.get(text(index - 1)).orNull
  }

  def search(text: String): Option[Result] = {
    var v = root
    var i = minLen
    var j = 0

    while (i <= text.length) {
      var v_ = getChild(v, text, i - j)
      while (v_ != null) {
        v = v_

        if (v.terminal) {
          return Some(Result(i - j - 1, text.substring(i - j - 1, i + v.depth - j - 1)))
        }

        j = j + 1
        v_ = getChild(v, text, i - j)
      }

      i = i + Math.min(v.shift2, Math.max(v.shift1, char(text(i - j - 1)) - j - 1))
      j = 0
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

    val allNodes = root.descendantsAndSelf

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
        n.depth,
        () => set1(n).map(convert),
        () => set2(n).map(convert),
        () => n.parentOpt.map(convert))
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
      c => lookup.getOrElse(c, minDepth)
    }

    def init(node: CWSearchNode): Unit = {
      // initialise lazy props
      node.set1
      node.set2
      node.parent
      node.children.valuesIterator.foreach(init)
    }

    def result(): CWSearch = {
      val converted = convert(root)
      init(converted)
      new CWSearch(minDepth, converted, char)
    }

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