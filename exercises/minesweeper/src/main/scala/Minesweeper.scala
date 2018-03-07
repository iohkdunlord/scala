import shapeless._
import syntax.sized._
import scala.language.dynamics
import shapeless.ops.nat._

object Minesweeper {

    type Row[W <: Nat] = Sized[IndexedSeq[Char], W]
    object Row {
      val empty: Row[_0] = Sized().asInstanceOf[Row[_0]]
    }

    type Board[W <: Nat, H <: Nat] = Sized[IndexedSeq[Row[W]], H]
    object Board {
      val empty: Board[_0, _0] = Sized[IndexedSeq]()
    }

    def annotate[W <: Nat : ToInt, H <: Nat : ToInt](b: Board[W, H]): Board[W, H] = {
      val w = Nat.toInt[W]
      val h = Nat.toInt[H]

      def count(r0: Int, c0: Int): Int =
        (for {
          r <- (r0 - 1) to (r0 + 1) if r >=0 && r < h
          c <- (c0 - 1) to (c0 + 1) if c >=0 && c < w
        } yield b.unsized(r).unsized(c)).count(_ == '*')

      var r = -1
      b map {row =>
        r += 1
        var c = -1
        row map {
          case '*' =>
            c += 1
            '*'
          case _ =>
            c += 1
            count(r, c) match {
              case 0 => '-'
              case n => n.toString.head
            }
        }
      }
    }
}

class RowBuilder[N <: Nat](row: Minesweeper.Row[N]) extends Dynamic {
  def selectDynamic(name: String): RowBuilder[Succ[N]] =
    new RowBuilder(row :+ name.head)

  def | = row
}

object | extends RowBuilder[_0](Minesweeper.Row.empty)
