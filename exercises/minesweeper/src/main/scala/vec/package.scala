import shapeless._
import syntax.sized._
import shapeless.ops.nat._

package object vec {

  type Vec[N <: Nat, T] = Sized[IndexedSeq[T], N]
  object Vec extends SizedBuilder[IndexedSeq] {
    def empty[T]: Vec[_0, T] = Sized[IndexedSeq]()
    def indexes[N <: Nat : ToInt]: Vec[N, Int] =
      (0 until Nat.toInt[N]).toVector.sized[N] match {
        case Some(r) =>
          r
        case None =>
          throw new RuntimeException("Impossible case encountered") // Since a range from 0 until N is, by definition, a collection with N
                                                                    // elements, this case should be impossible, unless there is a defect
                                                                    // on shapeless itself
      }
  }

  implicit class VecOps[N <: Nat, T](val vec: Vec[N, T]) extends AnyVal {

    def vzip[U](that: Vec[N, U])(implicit zipper: VZipper[N]): Vec[N, (T, U)] =
      zipper.zip(vec, that)

    def vzipWithIndex(implicit zipper: VZipper[N], toInt: ToInt[N]): Vec[N, (T, Int)] =
      vec vzip Vec.indexes[N]

  }

}


