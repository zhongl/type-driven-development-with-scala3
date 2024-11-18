import scala.compiletime.ops.int.S

enum Vect[N <: Int, +A]:
  case `[]`                                   extends Vect[0, Nothing]
  case ::[N <: Int, +A](x: A, xs: Vect[N, A]) extends Vect[S[N], A]

  def ::[B >: A](b: B) = new ::(b, this)
end Vect

object Vect:
  type +[M <: Int, N <: Int] <: Int = M match
    case 0    => N
    case S[m] => m + S[N]

  type Aux[A]                                                 = [N <: Int] =>> Vect[N, A]
  opaque type Plus[F[_ <: Int], M <: Int, N <: Int, O <: Int] = (F[M], F[N]) => F[O]

  given [N <: Int, A]: Plus[Aux[A], 0, N, N] = (_, ys) => ys
  given [M <: Int, N <: Int, A](using
    f: Plus[Aux[A], M, N, M + N]
  ): Plus[Aux[A], S[M], N, S[M + N]] =
    case (x :: xs, ys) => x :: f(xs, ys)

  extension [A, N <: Int](xs: Vect[N, A])
    def ++[B >: A, M <: Int](ys: Vect[M, B])(using
      f: Plus[Aux[B], N, M, M + N]
    ): Vect[M + N, B] = f(xs, ys)

end Vect

import Vect.*

val xs = 1 :: `[]`

xs ++ `[]`

(`[]` ++ xs)

xs ++ xs

object miles:
  // Copy from https://github.com/milessabin/strangeloop-2013/blob/master/src/main/scala/strangeloop/vect.scala#L55
  sealed trait Concat[A, XS, YS]:
    type Sum <: Int
    type Out = Vect[Sum, A]
    def apply(xs: XS, ys: YS): Out
  end Concat
  object Concat:
    given nil[A, N <: Int]: Concat[A, Vect[0, A], Vect[N, A]] with
      type Sum = N
      def apply(xs: Vect[0, A], ys: Vect[N, A]): Out = ys
    end nil

    given cons[A, M <: Int, N <: Int](using
      cc: Concat[A, Vect[M, A], Vect[N, A]]
    ): Concat[A, Vect[S[M], A], Vect[N, A]] with
      type Sum = S[cc.Sum]
      def apply(xs: Vect[S[M], A], ys: Vect[N, A]): Out =
        (xs: @unchecked) match
          case h :: t => h :: (cc(t, ys))
    end cons
  end Concat

  def concat[A, M <: Int, N <: Int](xs: Vect[M, A], ys: Vect[N, A])(using
    cc: Concat[A, Vect[M, A], Vect[N, A]]
  ) = cc(xs, ys)

end miles

miles.concat(xs, xs)

object idris:
  // TODO Path-dependent GADT reasoning
  // sealed infix trait Plus[M <: Int, N <: Int]:
  //   type O <: Int
  // object Plus:
  //   final case class Done[N <: Int]() extends Plus[0, N]:
  //     type O = N
  //   final case class Cont[M <: Int, N <: Int](p: M Plus N) extends Plus[S[M], N]:
  //     type O = S[p.O]

  //   given done[N <: Int]: Plus[0, N]                                 = Done()
  //   given cont[M <: Int, N <: Int](using p: M Plus N): Plus[S[M], N] = Cont(p)
  // end Plus

  enum Plus[M <: Int, N <: Int, O <: Int]:
    case Done[N <: Int]()                               extends Plus[0, N, N]
    case Cont[M <: Int, N <: Int](p: Plus[M, N, M + N]) extends Plus[S[M], N, S[M + N]]
  object Plus:
    given done[N <: Int]: Plus[0, N, N]                                                 = Done()
    given cont[M <: Int, N <: Int](using p: Plus[M, N, M + N]): Plus[S[M], N, S[M + N]] = Cont(p)

  import Plus.*

  // TODO
  // def concat[A, M <: Int, N <: Int]: (Vect[M, A], Vect[N, A]) => (p: M Plus N) ?=> Vect[p.O, A] =
  //   (a, b) =>
  //     (a, b, p) match
  //       case (`[]`, ys, Done())                    => ys
  //       case (x :: xs, ys, Cont(given (m Plus n))) => x :: concat(xs, ys)

  def concat[A, M <: Int, N <: Int, O <: Int]: (Vect[M, A], Vect[N, A]) => Plus[M, N, O] ?=> Vect[O, A] =
    (a, b) =>
      (a, b, summon[Plus[M, N, O]]) match
        case (`[]`, ys, Done())                       => ys
        case (x :: xs, ys, Cont(given Plus[m, n, o])) => x :: concat(xs, ys)
end idris

idris.concat(xs, xs)
