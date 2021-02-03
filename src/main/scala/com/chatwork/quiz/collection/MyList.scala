package com.chatwork.quiz.collection

import com.chatwork.quiz.{ MyNone, MyOption, MySome }

sealed trait MyList[+A] {

  // Easy
  def length: Int = this.foldLeft(0)((z, _) => z + 1)

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case MyNil        => z
      case MyCons(h, t) => t.foldLeft(f(z, h))(f)
    }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = this.reverse.foldLeft(z)((z, a) => f(a, z))

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = this match {
    case MyNil        => MyCons(b, MyNil)
    case MyCons(_, _) => MyCons(b, this)
  }
  // scalastyle:on

  // Normal
  def reverse: MyList[A] = this.foldLeft(MyNil: MyList[A])((z, a) => MyCons(a, z))

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] =
    this.foldRight(b)((a, z) => MyCons(a, z))
  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] =
    this.foldRight(MyNil: MyList[B])((a, z) => MyCons(f(a), z))

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] =
    this.foldRight(MyNil: MyList[B])((a, z) => f(a) ++ z)

  // Normal
  def filter(f: A => Boolean): MyList[A] =
    this.foldRight(MyNil: MyList[A]) { (a, z) =>
      if (f(a))
        MyCons(a, z)
      else
        z
    }

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  // TODO わからない！
  def withFilter(f: A => Boolean): MyList[A] = this.filter(f)

  // Normal
  def find(f: A => Boolean): MyOption[A] =
    this.foldLeft(MyNone: MyOption[A]) { (z, a) =>
      if (f(a))
        MySome(a)
      else
        z
    }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = prefix match {
    case MyNil => true
    case MyCons(ph, pt) =>
      this match {
        case MyNil        => false
        case MyCons(h, t) => if (ph == h) t.startsWith(pt) else false
      }
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = as.foldRight(MyNil: MyList[A])((b, z) => MyCons(b, z))

}
