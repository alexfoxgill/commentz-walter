package com.alexfoxgill.commentzwalter

import com.alexfoxgill.commentzwalter.Trie.Vertex

import scala.collection.mutable

private[commentzwalter] sealed abstract class Trie[A](val depth: Int) {
  private val children = mutable.Map.empty[A, Vertex[A]]
  private var terminal = false

  def isTerminal: Boolean = terminal

  def add(word: List[A]): Unit = {
    val leaf = word.foldLeft(this)(_ getOrAdd _)
    leaf.terminal = true
  }

  private def getOrAdd(child: A): Vertex[A] =
    children.getOrElseUpdate(child, Vertex(child, this))

  def get(child: A): Option[Vertex[A]] =
    children.get(child)

  def childNodes = children.values

  def descendantsAndSelf: Iterator[Trie[A]] =
    Iterator(this) ++ children.valuesIterator.flatMap(_.descendantsAndSelf)

  val parentOpt: Option[Trie[A]]
}

private[commentzwalter] object Trie {
  case class Root[A]() extends Trie[A](0) {
    val parentOpt = Option.empty
  }
  case class Vertex[A](value: A, parent: Trie[A]) extends Trie[A](parent.depth + 1) {
    val parentOpt = Some(parent)
  }
}