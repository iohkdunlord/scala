import shapeless._
import syntax.sized._
import shapeless.ops.nat._
import vec._
import MinesweeperDomain._

object MinesweeperDomain {

  sealed trait OutputBoardCell
  sealed trait InputBoardCell extends OutputBoardCell

  object BoardCell {
    case object __ extends InputBoardCell
    case object _M extends InputBoardCell

    case object _1 extends OutputBoardCell
    case object _2 extends OutputBoardCell
    case object _3 extends OutputBoardCell
    case object _4 extends OutputBoardCell
    case object _5 extends OutputBoardCell
    case object _6 extends OutputBoardCell
    case object _7 extends OutputBoardCell
    case object _8 extends OutputBoardCell

    def apply(count: Int): Option[OutputBoardCell] =
      count match {
        case 0 => Some(__)
        case 1 => Some(_1)
        case 2 => Some(_2)
        case 3 => Some(_3)
        case 4 => Some(_4)
        case 5 => Some(_5)
        case 6 => Some(_6)
        case 7 => Some(_7)
        case 8 => Some(_8)
        case _ => None
      }
  }

  type IRow[W <: Nat] = Vec[W, InputBoardCell]
  object IRow {
    val empty: IRow[_0] = Vec.empty
  }

  type ORow[W <: Nat] = Vec[W, OutputBoardCell]
  object ORow {
    val empty: ORow[_0] = Vec.empty
  }

  type IBoard[W <: Nat, H <: Nat] = Vec[H, IRow[W]]
  object IBoard {
    val empty: IBoard[_0, _0] = Vec.empty
  }

  type OBoard[W <: Nat, H <: Nat] = Vec[H, ORow[W]]
  object OBoard {
    val empty: OBoard[_0, _0] = Vec.empty
  }

}

object Minesweeper {

    def annotate[W <: Nat : ToInt : VZipper, H <: Nat : ToInt : VZipper](b: IBoard[W, H]): OBoard[W, H] = {
      val w = Nat.toInt[W]
      val h = Nat.toInt[H]

      def count(r0: Int, c0: Int): Int =
        (for {
          r <- (r0 - 1) to (r0 + 1) if r >=0 && r < h
          c <- (c0 - 1) to (c0 + 1) if c >=0 && c < w
        } yield b.unsized(r).unsized(c)).count(_ == BoardCell._M)

      (b.vzipWithIndex) map {case (row, r) =>
        (row.vzipWithIndex) map {
          case (BoardCell._M, _) => BoardCell._M
          case (_ , c) =>
            BoardCell(count(r, c)).getOrElse(throw new RuntimeException("Impossible case, the maximum amount of cells is 8"))
        }
      }
    }
}

