package vec

import shapeless._
import syntax.sized._
import shapeless.ops.nat._


trait VZipper[N <: Nat] {
  def zip[A, B](as: Vec[N, A], bs: Vec[N, B]): Vec[N, (A, B)]
}

object VZipper {

  def apply[N <: Nat : VZipper]: VZipper[N] =
    implicitly

  implicit val VZipperForEmpty: VZipper[_0] =
    new VZipper[_0] {
      def zip[A, B](as: Vec[_0, A], bs: Vec[_0, B]): Vec[_0, (A, B)] =
        Vec.empty
    }

  implicit def VZipperForNonEmpy[N <: Nat : VZipper]: VZipper[Succ[N]] =
    new VZipper[Succ[N]] {
      def zip[A, B](as: Vec[Succ[N], A], bs: Vec[Succ[N], B]): Vec[Succ[N], (A, B)] =
        (as.head, bs.head) +: (VZipper[N].zip(as.tail, bs.tail))
    }
}
