package com.chatwork.quiz.misc

import scala.io.Source

/**
  * ワードをカウントするオブジェクト。
  */
object WordCounter {

  /**
    * 文字列から単語数をカウントする。
    *
    * @param words 文字列
    * @return 単語がキー、単語数がヴァリューのマップ
    */
  def countWords(words: List[String]): Map[String, Int] =
    words
      .flatMap(_.split(" ").toList)
      .foldLeft(Map.empty[String, Int]) { (z, a) =>
        if (z.contains(a))
          z.updated(a, z(a) + 1)
        else
          z.updated(a, 1)
      }

  def trueCountWords(): Unit = {
    val source = Source.fromFile("/home/nori/Desktop/word_count.txt")

    val lines = source.getLines
    val words = lines.foldLeft(Nil: List[String]) { (z, a) =>
      a.
        replace(",", "").
        replace(".", "").
        toLowerCase :: z
    }

    println(countWords(words))

    source.close
  }
}
