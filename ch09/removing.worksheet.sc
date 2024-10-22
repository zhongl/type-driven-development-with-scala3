import scala.annotation.implicitNotFound
import scala.compiletime.ops.int.S

enum Vect[N <: Int, +A]:
  case Nil extends Vect[0, Nothing]
  case Cons[N <: Int, +A, H <: A, T <: Vect[N, A]] private[Vect] (
    head: H,
    tail: T
  ) extends Vect[S[N], A]
  def ::[H >: A](head: H) = new Cons[N, H, head.type, this.type](head, this)

object Vect:

  @implicitNotFound("Can't remove element from the vect does't contain")
  sealed trait Removable[N <: Int, A, X <: A, -XS <: Vect[S[N], A]]:
    extension (xs: XS) def removed: Vect[N, A]

  given [A, N <: Int, X <: A]: Removable[N, A, X, Cons[N, A, X, ?]] with
    extension (xs: Cons[N, A, X, ?]) def removed = xs.tail

  given [A, N <: Int, X <: A, XS <: Vect[S[N], A], YS <: Cons[S[N], A, ?, XS]](using
    r: Removable[N, A, X, XS]
  ): Removable[S[N], A, X, YS] with
    extension (xs: YS) def removed = xs.head :: r.removed(xs.tail)

  extension [N <: Int, A, XS <: Cons[N, A, ?, ?]](xs: XS)
    def -(x: A)(using Removable[N, A, x.type, XS]): Vect[N, A] = xs.removed
end Vect

import Vect.*

val v1 = 1 :: Nil

v1 - 1

//v1 - 2

(3 :: 2 :: v1) - 1

("a" :: Nil) - "a"
