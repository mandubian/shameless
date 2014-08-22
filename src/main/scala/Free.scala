import scalaz._
import Scalaz._

import shapeless._
import poly._

sealed abstract class FreeView[S[_], A]

object FreeView {
  case class Pure[S[_], A](a: A) extends FreeView[S, A]
  case class Impure[S[_], A0, F <: HFree[S] { type A = A0 }](a: S[F]) extends FreeView[S, A0]
}

sealed trait HFree[S[_]] {
  type A
  type X
  type FH <: FreeView[S, X]
  type FT <: FingerTree

  val head: FH
  val tail: FT

}

object HFree {

  import FreeView._

  type Free[S[_], A0] = HFree[S] {
    type A = A0
  }

  type Aux[S[_], A0, X0, FH0 <: FreeView[S, X0], FT0 <: FingerTree] = HFree[S] {
    type A = A0
    type X = X0
    type FH = FH0
    type FT = FT0
  }

  type FC[S[_], A, B, Out <: Free[S, B]] = A => Out

  // trait FC[S[_], A, B, Out0 <: Free[S, B]] extends DepFn1[A] { type Out = Out0 }

  // object FC {
  //   def apply[S[_], A, B, Out0 <: Free[S, B]](f: A => Out0) = new FC[S, A, B, Out0] {
  //     def apply(a: A): Out0 = f(a)
  //   }
  // }

  implicit class FreeOps[S0[_], A0, X0, FH0 <: FreeView[S0, X0], FT0 <: FingerTree](
    val free: HFree.Aux[S0, A0, X0, FH0, FT0]
  ) extends AnyVal {

    def toView[Out <: FreeView[S0, _]]()(
      implicit view: HFree.ToView.Case.Aux[HFree.Aux[S0, A0, X0, FH0, FT0], Out]
    ): Out = view(free)


    def bind[B, X1, FH1 <: FreeView[S0, X1], Out <: FingerTree, Out2 <: FingerTree](f: A0 => HFree.Aux[S0, B, X1, FH1, Out])(
      implicit add: FingerTree.Add.Case.Aux[
                      FT0, HNil, FingerTree.Single[FC[S0, A0, B, HFree.Aux[S0, B, X1, FH1, Out]]],
                      Out2
                    ]
    ) = {
      new HFree[S0] {
        type A = B
        type X = X0
        type FH = FH0
        type FT = Out2

        val head: FH0 = free.head
        val tail = add(
          free.tail ::
          HNil ::
          FingerTree.Single[FC[S0, A0, B, HFree.Aux[S0, B, X1, FH1, Out]]](f) ::
          HNil
        )
      }
    }

  }

  def apply[S[_], A0, X0, FH0 <: FreeView[S, X0], FT0 <: FingerTree](head0: FH0, tail0: FT0): HFree.Aux[S, A0, X0, FH0, FT0] =
    new HFree[S] {
      type A = A0
      type X = X0
      type FH = FH0
      type FT = FT0

      val head = head0
      val tail = tail0
    }


  def point[S[_], A](a: A): Aux[S, A, A, Pure[S, A], FingerTree.Empty.type] = 
    fromView[S, A, Pure[S, A]](Pure[S, A](a))

  def fromView[S[_], A0, FH0 <: FreeView[S, A0]](v: FH0) = 
    new HFree[S] {
      type A = A0
      type X = A0
      type FH = FH0
      type FT = FingerTree.Empty.type

      val head = v
      val tail = FingerTree.Empty
    }

  trait HFunctor[S[_], A, B, In <: S[A], Out <: S[B]] {
    def map(s: In)(f: A => B): Out
  }

  object IsFC extends Poly1 {
    implicit def caseX[S[_], X, A, X1, FH1 <: FreeView[S, X1], FT1 <: FingerTree]: 
      Case.Aux[FC[S, X, A, HFree.Aux[S, A, X1, FH1, FT1]], FC[S, X, A, HFree.Aux[S, A, X1, FH1, FT1]]] =
      at[FC[S, X, A, HFree.Aux[S, A, X1, FH1, FT1]]] { a => a }
  }

  object ToView extends Poly1 {

    implicit def casePureEmpty[
      S[_], A
    ]: Case.Aux[HFree.Aux[S, A, A, Pure[S, A], FingerTree.Empty.type], Pure[S, A]] =
      at[HFree.Aux[S, A, A, Pure[S, A], FingerTree.Empty.type]] { free =>
        Pure(free.head.a)
      }

    implicit def casePureSingle[
      S[_], A, X,
      //X1, FH1 <: FreeView[S, X1], FT1 <: FingerTree,
      Out <: Free[S, A],
      Out2 //<: FreeView.Pure[S, X1]
    ](
      implicit tv: ToView.Case.Aux[Out, Out2]
    ): Case.Aux[
      HFree.Aux[
        S, A, X,
        Pure[S, X],
        FingerTree.Single[FC[S, X, A, Out]]
      ],
      Out2
    ] = at[HFree.Aux[
      S, A, X,
      Pure[S, X],
      FingerTree.Single[FC[S, X, A, Out]]
    ]] { free =>
      tv(free.tail.a(free.head.a))
    }

    implicit def casePureTree[
      S[_], A, X, B,
      PR <: FingerTree.Digit, MD <: FingerTree, SF <: FingerTree.Digit,
      X1, FH1 <: FreeView[S, X1], FT1 <: FingerTree,
      HA, HL <: HList, T1 <: FingerTree,
      Out1 <: FingerTree,
      Out2 <: FreeView[S, B]
    ](implicit  toList: FingerTree.Digit.ToList.Case.Aux[PR, HA :: HL],
                isFC: IsFC.Case.Aux[HA, FC[S, X, A, HFree.Aux[S, A, X1, FH1, FT1]]],
                deepl: FingerTree.DeepL.Case.Aux[HL, MD, SF, T1],
                //treeView: FingerTree.ViewL.Case.Aux[
                //            FingerTree.Deep[PR, MD, SF],
                //            FC[S, X, A, HFree.Aux[S, A, X1, FH1, FT1]] :: T1 :: HNil
                //          ],
                add: FingerTree.Add.Case.Aux[FT1, HNil, T1, Out1],
                view: ToView.Case.Aux[HFree.Aux[S, A, X1, FH1, Out1], Out2]
    ): Case.Aux[HFree.Aux[S, A, X, Pure[S, X], FingerTree.Deep[PR, MD, SF]], Out2] =
      at[HFree.Aux[S, A, X, Pure[S, X], FingerTree.Deep[PR, MD, SF]]] { free =>
        //val tailView = treeView(free.tail)
        //val fn = tailView.head
        val fn :: tail = toList(free.tail.prefix)
        val tree = deepl(tail :: free.tail.middle :: free.tail.suffix :: HNil)
        val fn1 = isFC(fn)
        val nextFree = fn1(free.head.a)
        //val newFree = HFree[S, A, X1, FH1, Out1](nextFree.head, add(nextFree.tail :: HNil :: tailView.tail.head :: HNil))
        val newFree = HFree[S, A, X1, FH1, Out1](nextFree.head, add(nextFree.tail :: HNil :: tree :: HNil))
        view(newFree)
      }


    // implicit def caseImpureEmpty[
    //   S[_], A, FT <: FingerTree,
    //   X1, FH1 <: FreeView[S, X1], FT1 <: FingerTree, T1 <: FingerTree,
    //   Out1 <: FingerTree
    // ](implicit
    //   F: HFunctor[S,
    //        HFree.Aux[S, A, X1, FH1, FT1],
    //        HFree.Aux[S, A, X1, FH1, Out1],
    //        S[HFree.Aux[S, A, X1, FH1, FT1]],
    //        S[HFree.Aux[S, A, X1, FH1, Out1]]
    //      ],
    //   add: FingerTree.Add.Case.Aux[FT1, HNil, FT, Out1]
    // ): Case.Aux[
    //   HFree.Aux[S, A, A, Impure[S, A, HFree.Aux[S, A, X1, FH1, FT1]], FT],
    //   Impure[S, A, HFree.Aux[S, A, X1, FH1, Out1]]
    // ] = 
    //   at[HFree.Aux[S, A, A, Impure[S, A, HFree.Aux[S, A, X1, FH1, FT1]], FT]] { free =>

    //     Impure[S, A, HFree.Aux[S, A, X1, FH1, Out1]]( 
    //       F.map(
    //         free.head.a
    //       ){ next : HFree.Aux[S, A, X1, FH1, FT1] => 
    //         HFree[S, A, X1, FH1, Out1](next.head, add(next.tail :: HNil ::  free.tail :: HNil))
    //       }
    //     )
    //   }

  }

}
