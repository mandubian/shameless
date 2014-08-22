/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda, Id}

import scala.concurrent._
import shapeless.test._
import scala.collection.{ GenTraversable, GenTraversableLike }
import scala.collection.generic.CanBuildFrom


class FreeSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import HFree._


  "Free" should "compile" in {
    val free = HFree.point[Id, Int](5)
    free.toView()

    val free2 =
      free bind { (i:Int) =>
        HFree.point[Id, String](i.toString)
      } bind { (s:String) =>
        HFree.point[Id, Int](s.toInt)
      }

    // val f = HFree[
    //   Id, String, Int,
    //   FreeView.Pure[Id, Int],
    //   FingerTree.Single[
    //     HFree.FC[
    //       Id, Int, String,
    //       HFree.Aux[
    //         Id, String, String,
    //         FreeView.Pure[Id, String],
    //         FingerTree.Empty.type
    //       ]
    //     ]
    //   ]
    // ](
    //   FreeView.Pure(5),
    //   FingerTree.Single(FC[Id, Int, String, HFree.Aux[
    //         Id, String, String,
    //         FreeView.Pure[Id, String],
    //         FingerTree.Empty.type
    //       ]]{ (i:Int) => HFree.point[Id, String](i.toString) })
    // )

    // val f2 = HFree[
    //   Id, String, String,
    //   FreeView.Pure[Id, String],
    //   FingerTree.Empty.type
    // ](FreeView.Pure("toto"), FingerTree.Empty)

    println("FV:"+free2.toView())

  }

}