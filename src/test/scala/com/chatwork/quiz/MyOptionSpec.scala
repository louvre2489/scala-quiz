package com.chatwork.quiz

import org.scalatest.{Matchers, FunSpec}

class MyOptionSpec extends FunSpec with Matchers {

  describe("MyOption#get") {
    it("should return a value if it's not empty") {
      MySome(100).get shouldBe 100
    }
    it("should throw a NoSuchElementException if it's empty") {
      intercept[NoSuchElementException] {
        MyNone.get
      }
    }
  }

  describe("MyOption#isEmpty") {
    it("should be true if it is empty") {
      MyNone.isEmpty shouldBe true
    }
    it("should be false if it contains a value") {
      MySome(100).isEmpty shouldBe false
    }
  }

  describe("MyOption#map") {
    it("should return a MySome containing the result of applying f") {
      MySome(100).map(_ * 2) shouldBe MySome(200)
    }
    it("should return MyNone if it is empty") {
      MyNone.map(_ => false) shouldBe MyNone
    }
  }

  describe("MyOption#flatMap") {
    it("should return the result of applying f") {
      MySome(100).flatMap(e => MySome(e * 2)) shouldBe MySome(200)
    }
    it("should return MyNone if it is empty") {
      MyNone.flatMap(_ => MySome(false)) shouldBe MyNone
    }
  }

  describe("MyOption#filter") {
    it("should return MySome if the predicate returns true") {
      MySome(100).filter(_ > 0) shouldBe MySome(100)
    }
    it("should return MyNone if the predicate returns false") {
      MySome(100).filter(_ < 0) shouldBe MyNone
    }
  }

  describe("MyOption#getOrElse") {
    it("should return the MyOption's value if it contains a value") {
      MySome(100).getOrElse(20) shouldBe 100
    }
    it("should return the default value if it is empty") {
      MyNone.getOrElse(20) shouldBe 20
    }
  }

  describe("MyOption#orElse") {
    it("should return this MyOption value if it is not empty") {
      MySome(100).orElse(MySome(20)) shouldBe MySome(100)
    }
    it("should return the default value if it is empty") {
      MyNone.orElse(MySome(20)) shouldBe MySome(20)
    }
  }

}
