import scala.annotation.implicitNotFound
import scala.compiletime.ops.int.S

enum Vect[N <: Int, +A]:
  case `[]`                                                                    extends Vect[0, Nothing]
  case ::[N <: Int, +A, +H <: A, +T <: Vect[N, A]] private[Vect] (x: H, xs: T) extends Vect[S[N], A]

  def ::[X >: A](x: X) = new ::[N, X, x.type, this.type](x, this)

object Vect:
  extension [N <: Int, A, XXS <: Vect[S[N], A], XS <: Vect[N, A]](xs: XXS)
    def -(x: A)(using f: Remove[N, x.type, XXS, XS]): XS = f(xs)

  @implicitNotFound("Can't remove element from the vect does't contain")
  opaque type Remove[N <: Int, X, -XXS <: Vect[S[N], ?], +XS <: Vect[N, ?]] = XXS => XS
  object Remove:
    given here[A, N <: Int, X <: A, XS <: Vect[N, A]]: Remove[N, X, ::[N, A, X, XS], XS] =
      case _ :: xs => xs

    given threre[A, N <: Int, X <: A, Y <: A, XS <: Vect[N, A], YS <: Vect[S[N], A]](using
      f: Remove[N, X, YS, XS]
    ): Remove[S[N], X, ::[S[N], A, Y, YS], ::[N, A, Y, XS]] =
      case x :: xs => x :: f(xs)
  end Remove

end Vect

import Vect.*

val v1 = 2 :: 1 :: `[]`

v1 - 2 - 1

v1 - 1 // - 2 // error

val v2 = 3 :: v1

v2 - 1

("a" :: `[]`) - "a"

object Idris:
  enum Elem[N <: Int, A, X <: A, +XS <: Vect[S[N], A]]:
    case Here[N <: Int, A, X <: A, XS <: Vect[N, A]]() extends Elem[N, A, X, ::[N, A, X, XS]]
    case There[N <: Int, A, X <: A, Y <: A, XS <: Vect[S[N], A]](
      later: Elem[N, A, X, XS]
    ) extends Elem[S[N], A, X, ::[S[N], A, Y, XS]]
  object Elem:
    given here[N <: Int, A, X <: A, XS <: Vect[N, A]]: Elem[N, A, X, ::[N, A, X, XS]] = Here()
    given there[N <: Int, A, X <: A, Y <: A, XS <: Vect[S[N], A]](using
      later: Elem[N, A, X, XS]
    ): Elem[S[N], A, X, ::[S[N], A, Y, XS]] = There(later)

  import Elem.*

  def has[N <: Int, A, XS <: Vect[S[N], A]](x: A, xs: XS)(using
    e: Elem[N, A, x.type, XS]
  ): Boolean = true

  def remove[N <: Int, A, XS <: Vect[S[N], A]](x: A, xs: XS)(using
    e: Elem[N, A, x.type, XS]
  ): Vect[N, A] = (e, xs) match
    case (Here(), _ :: ys)            => ys
    case (There(later), y :: z :: ys) => y :: remove(x, z :: ys)(using later)
    case _                            => ???

  type Remove[X, XS <: Vect[?, ?]] <: Vect[?, ?] = XS match
    case ::[?, ?, X, ys]    => ys
    case ::[S[n], a, y, xs] => ::[n, a, y, Remove[X, xs]]
end Idris

Idris.has(2, 1 :: 2 :: `[]`)

Idris.remove(1, 1 :: 2 :: `[]`)
