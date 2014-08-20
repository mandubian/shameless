/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda}

import scala.concurrent._
import shapeless.test._
import scala.collection.{ GenTraversable, GenTraversableLike }
import scala.collection.generic.CanBuildFrom


class FTreeCompileSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import FingerTree._

  case class Toto(a: String, b: Float)

  "FingerTree" should "compile" in {
    val t1 =
      FingerTree.Empty :+
// 00
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 25
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 50
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 75
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 100
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 125
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 150
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// 175
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
      1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 200
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 225
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 250
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 275
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 300
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 325
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 350
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 375
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 400
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 425
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 450
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 475
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 500
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 525
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 550
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 575
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 600
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 625
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 650
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 675
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 700
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 725
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 750
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 775
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 800
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 825
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 850
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 875
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 900
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 925
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 950
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 975
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
//       1 :+ "toto" :+ true :+ 1.234 :+ Toto("tutu", 9.87f) :+
// // 1000
      FingerTree.Empty

      println("t1:"+t1)
  }
}


/*
 COMPILE DURATION:
PREPEND   => [success] Total time: 13 s, completed 20 août 2014 12:33:28
APPEND    => [success] Total time: 12 s, completed 20 août 2014 12:35:00
*/