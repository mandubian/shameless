import scalaz._
import Scalaz._

import shapeless._
import poly._

object FreeT {

  type FC[S[_], A, B] = A => Free[S, B]
  type FMExp[S[_], A, B] = FingerTree

  sealed abstract class FreeView[S[_], A]

  object FreeView {
    case class Pure[S[_], A](a: A) extends FreeView[S, A]
    case class Impure[S[_], A](a: S[FreeT.Free[S, A]]) extends FreeView[S, A]
  }

  sealed trait Free[S[_], A] {
    type X
    type FH <: FreeView[S, X]
    type FT <: FingerTree

    val head: FH
    val tail: FT
  }

  object Free {
    import FreeView._

    def fromView[S[_], A, FH0 <: FreeView[S, A]](v: FH0) = new Free[S, A] {
      type X = A
      type FH = FH0
      type FT = FingerTree.Empty.type

      val head = v
      val tail = FingerTree.empty()
    }

    // object ToView extends Poly2 {
    //   implicit def caseEmpty[S[_], A]: Case.Aux[FreeView[S, A], FingerTree.Empty.type, FreeView[S, A]] =
    //     at[FreeView[S, A], FingerTree.Empty.type] { (v, a) => Pure(a) }
    // }

    // def toView[S[_], A](free: Free[S, A])(
    //   implicit toView: ToView.Case.Aux[free.FH, free., Out]
    // ): FreeView[S, A] = {
    //   free.head match {
    //     case Pure(x) =>

    //     case Impure(f)
    //   }
    // }

    def point[S[_], A](a: A) = fromView[S, A, Pure[S, A]](Pure[S, A](a))

    // def bind[S[_], A, B, Out <: FingerTree](free: Free[S, A])(f: A => Free[S, B])(
    //   implicit add: FingerTree.Add.Case.Aux[free.FT, HNil, FingerTree.Single[FC[S, A, B]], Out]
    // ): Free[S, B] = {
    //   new Free[S, B] {
    //     type X = free.X
    //     type FH = free.FH
    //     type FT = Out

    //     val head = free.head
    //     val tail = add(free.tail :: HNil :: FingerTree.Single[FC[S, A, B]](f) :: HNil)
    //   }
    // }
  }
}