/*
 * Copyright 2023 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porcupine

import cats.ContravariantMonoidal
import cats.Monoid
import cats.arrow.Profunctor
import cats.data.State
import cats.syntax.all.*
import scala.quoted.Expr
import scala.quoted.Exprs
import scala.quoted.Quotes
import scala.quoted.Varargs

final class Query[A, B](val sql: String, val encoder: Encoder[A], val decoder: Decoder[B])

object Query:

  given Profunctor[Query] = new:
    def dimap[A, B, C, D](fab: Query[A, B])(f: C => A)(g: B => D) =
      Query(fab.sql, fab.encoder.contramap(f), fab.decoder.map(g))

final class Fragment[A](
    val part: Fragment.Part,
    val encoder: Encoder[A],
):
  def sql: String = part.compile.runA(1).value

  def command: Query[A, Unit] = Query(sql, encoder, Codec.unit)

  def query[B](decoder: Decoder[B]): Query[A, B] = Query(sql, encoder, decoder)

  def apply(a: A): Fragment[Unit] = Fragment(part, encoder.contramap(_ => a))

  def stripMargin: Fragment[A] = stripMargin('|')

  def stripMargin(marginChar: Char): Fragment[A] =
    Fragment(part.stripMargin(true, marginChar), encoder)

object Fragment:
  sealed trait Part:
    def compile: State[Int, String]
    def concatenate(other: Part): Part = other match {
      case Part.Concatenate(values) => Part.Concatenate(this :: values)
      case _ => Part.Concatenate(List(this, other))
    }
    def stripMargin(head: Boolean, marginChar: Char): Part

  object Part:
    final case class Literal(x: String) extends Part:
      def compile = State.pure(x)
      def stripMargin(head: Boolean, marginChar: Char) =
        if (head) Literal(x.stripMargin(marginChar))
        else Literal(x.takeWhile(_ != '\n') ++ x.dropWhile(_ != '\n').stripMargin(marginChar))
    final case class Concatenate(values: List[Part]) extends Part:
      def compile = values.traverse(_.compile).map(_.combineAll)
      override def concatenate(other: Part) = other match {
        case Concatenate(values) => Concatenate(this.values ++ values)
        case _ => Concatenate(this.values :+ other)
      }
      def stripMargin(head: Boolean, marginChar: Char) =
        values match {
          case h :: t =>
            Concatenate(
              h.stripMargin(head, marginChar) :: t.map(_.stripMargin(false, marginChar)),
            )
          case other => this
        }
    final case class Parameters(advance: State[Int, List[Int]]) extends Part:
      def compile = advance.map(_.map(idx => s"?$idx").mkString(", "))
      def stripMargin(head: Boolean, marginChar: Char) = this

  given ContravariantMonoidal[Fragment] = new:
    val unit = Fragment(Part.Concatenate(List.empty), Codec.unit)
    def product[A, B](fa: Fragment[A], fb: Fragment[B]) =
      Fragment(fa.part.concatenate(fb.part), (fa.encoder, fb.encoder).tupled)
    def contramap[A, B](fa: Fragment[A])(f: B => A) =
      Fragment(fa.part, fa.encoder.contramap(f))

  given Monoid[Fragment[Unit]] = new:
    def empty = ContravariantMonoidal[Fragment].unit
    def combine(x: Fragment[Unit], y: Fragment[Unit]) =
      (x, y).contramapN(_ => ((), ()))

extension (inline sc: StringContext)
  transparent inline def sql(inline args: Any*): Any =
    ${ sqlImpl('sc, 'args) }

private def sqlImpl(
    scExpr: Expr[StringContext],
    argsExpr: Expr[Seq[Any]],
)(using Quotes): Expr[Any] =

  val parts = scExpr match
    case '{ StringContext(${ Varargs(Exprs(parts)) }: _*) } => parts.toList.map(Expr(_))
    case _ => List.empty

  val args = Varargs.unapply(argsExpr).toList.flatMap(_.toList)

  // TODO appending to `List` is slow
  val fragment =
    parts.zipAll(args, '{ "" }, '{ "" }).foldLeft('{ List.empty[Fragment.Part] }) {
      case ('{ $acc: List[Fragment.Part] }, ('{ $p: String }, '{ $s: String })) =>
        '{ $acc :+ Fragment.Part.Literal($p) :+ Fragment.Part.Literal($s) }
      case ('{ $acc: List[Fragment.Part] }, ('{ $p: String }, '{ $e: Encoder[t] })) =>
        '{ $acc :+ Fragment.Part.Literal($p) :+ Fragment.Part.Parameters($e.parameters) }
      case ('{ $acc: List[Fragment.Part] }, ('{ $p: String }, '{ $f: Fragment[t] })) =>
        '{ $acc :+ Fragment.Part.Literal($p) :+ $f.part }
    }

  val encoder = args.collect {
    case '{ $e: Encoder[t] } => e
    case '{ $f: Fragment[t] } => '{ $f.encoder }
  } match
    case Nil => '{ Codec.unit }
    case '{ $e: Encoder[t] } :: Nil => e
    case many =>
      many.foldRight[Expr[Any]]('{ ContravariantMonoidal[Encoder].point(EmptyTuple) }) {
        case ('{ $head: Encoder[h] }, '{ $tail: Encoder[EmptyTuple] }) =>
          '{
            ($head, $tail).contramapN[h *: EmptyTuple] { case h *: EmptyTuple =>
              (h, EmptyTuple)
            }
          }
        case ('{ $head: Encoder[h] }, '{ $tail: Encoder[ht *: t] }) =>
          '{
            ($head, $tail).contramapN[h *: ht *: t] { case h *: t =>
              (h, t)
            }
          }
      }

  (fragment, encoder) match
    case ('{ $s: List[Fragment.Part] }, '{ $e: Encoder[a] }) =>
      '{ Fragment[a](Fragment.Part.Concatenate($s), $e) }
    case _ => sys.error("porcupine pricked itself")
