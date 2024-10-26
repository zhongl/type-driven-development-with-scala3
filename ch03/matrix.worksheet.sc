import scala.annotation.tailrec
import scala.compiletime.ops.int.S

enum Vect[N <: Int, +A]:
  case `[]`                                                       extends Vect[0, Nothing]
  case ::[N <: Int, +A] private[Vect] (head: A, tail: Vect[N, A]) extends Vect[S[N], A]

  def ::[B >: A](b: B) = new ::(b, this)
end Vect

object Vect:
  extension [N <: Int, A](xs: Vect[N, A])
    @tailrec
    def seq(acc: Seq[A] = Seq()): Seq[A] = xs match
      case `[]`   => acc
      case h :: t => t.seq(acc :+ h)

    def show(using f: Show[Dim[A], Vect[N, A]]) = f(xs)

  enum Nat[N <: Int]:
    case Zero                      extends Nat[0]
    case Succ[N <: Int](n: Nat[N]) extends Nat[S[N]]

    lazy val value: Int =
      @tailrec
      def rec(n: Nat[?], r: Int): Int = n match
        case Zero    => r
        case Succ(n) => rec(n, 1 + r)

      rec(this, 0)
  end Nat

  object Nat:
    given Nat[0]                                 = Zero
    given [N <: Int](using n: Nat[N]): Nat[S[N]] = Succ(n)

  type Dim[A] <: Int = A match
    case Vect[?, a] => S[Dim[a]]
    case _          => 0

  opaque type Show[D <: Int, -A] = A => String
  object Show:
    given zero[D <: Int, A]: Show[D, Vect[0, A]]   = _ => "[]"
    given one[N <: Int, A]: Show[0, Vect[N, A]] = _.seq().mkString("[", ", ", "]")
    given more[D <: Int, N <: Int, A](using f: Show[D, A], n: Nat[S[D]]): Show[S[D], Vect[S[N], A]] =
      val i   = n.value
      val tab = "  "
      val de  = tab * i
      val ac  = tab * (i - 1)
      _.seq().map(f).mkString(s"${ac}[\n${de}", s"\n${de}", s"\n${ac}]")
  end Show

  type Mat[R <: Int, C <: Int, A] = Vect[R, Vect[C, A]]
  extension [R <: Int, C <: Int, A](mat: Mat[R, C, A])
    def trans: Nat[C] ?=> Mat[C, R, A] =

      def zip[R <: Int, C <: Int, A]: (Vect[C, A], Mat[C, R, A]) => Mat[C, S[R], A] =
        case (`[]`, `[]`)       => `[]`
        case (x :: xs, y :: ys) => (x :: y) :: zip(xs, ys)

      mat match
        case `[]`    => fill[C](`[]`)
        case x :: xs => zip(x, xs.trans)

    end trans
  end extension

  def fill[N <: Int](using n: Nat[N]): [A] => A => Vect[N, A] = n match
    case Nat.Zero    => [A] => _ => `[]`
    case Nat.Succ(n) => [A] => a => a :: fill(using n)(a)
end Vect

import Vect.*

`[]`.show

(1 :: `[]`).show

val mat: Mat[3, 2, Int] = (1 :: 2 :: `[]`)
  :: (3 :: 4 :: `[]`)
  :: (5 :: 6 :: `[]`) :: `[]`

mat.show

mat.trans.show
