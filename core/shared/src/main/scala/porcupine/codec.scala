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

import cats.Applicative
import cats.ContravariantMonoidal
import cats.InvariantMonoidal
import cats.data.StateT
import cats.syntax.all.*
import org.typelevel.twiddles.TwiddleSyntax
import scodec.bits.ByteVector

trait Encoder[A]:
  def encode(a: A): List[LiteValue]

object Encoder:
  given ContravariantMonoidal[Encoder] = new:
    def unit = Codec.unit

    def product[A, B](fa: Encoder[A], fb: Encoder[B]) = new:
      def encode(ab: (A, B)) =
        val (a, b) = ab
        fa.encode(a) ::: fb.encode(b)

    def contramap[A, B](fa: Encoder[A])(f: B => A) = new:
      def encode(b: B) = fa.encode(f(b))

trait Decoder[A]:
  def decode: StateT[Either[Throwable, *], List[LiteValue], A]

object Decoder extends TwiddleSyntax[Decoder]:
  given decoderApplicative: Applicative[Decoder] = new:
    def pure[A](a: A) = new:
      def decode = StateT.pure(a)

    def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]) = new:
      def decode = ff.decode.ap(fa.decode)

    override def product[A, B](fa: Decoder[A], fb: Decoder[B]) = new:
      def decode = fa.decode.product(fb.decode)

    override def map[A, B](fa: Decoder[A])(f: A => B) = new:
      def decode = fa.decode.map(f)

trait Codec[A] extends Encoder[A], Decoder[A]:
  def asEncoder: Encoder[A] = this
  def asDecoder: Decoder[A] = this

object Codec extends TwiddleSyntax[Codec]:
  final class PrimitiveCodec[A](
      name: String,
      apply: A => LiteValue,
      unapply: PartialFunction[LiteValue, A],
  ) extends Codec[A]:
    outer =>
    override def encode(a: A): List[LiteValue] = apply(a) :: Nil
    override def decode: StateT[Either[Throwable, *], List[LiteValue], A] = StateT {
      case unapply(l) :: tail => Right((tail, l))
      case other => Left(new RuntimeException(s"Expected $name, got ${other.headOption}"))
    }
    val opt: Codec[Option[A]] = new:
      override def encode(a: Option[A]): List[LiteValue] =
        a.fold(LiteValue.Null)(outer.apply) :: Nil
      override def decode: StateT[Either[Throwable, *], List[LiteValue], Option[A]] = StateT {
        case outer.unapply(l) :: tail => Right((tail, Some(l)))
        case LiteValue.Null :: tail => Right((tail, None))
        case other =>
          Left(new RuntimeException(s"Expected $name or NULL, got ${other.headOption}"))
      }

  val integer: PrimitiveCodec[Long] =
    new PrimitiveCodec("integer", LiteValue.Integer.apply, { case LiteValue.Integer(i) => i })

  val real: PrimitiveCodec[Double] =
    new PrimitiveCodec("real", LiteValue.Real.apply, { case LiteValue.Real(r) => r })

  val text: Codec[String] =
    new PrimitiveCodec("text", LiteValue.Text.apply, { case LiteValue.Text(t) => t })

  val blob: Codec[ByteVector] =
    new PrimitiveCodec("blob", LiteValue.Blob.apply, { case LiteValue.Blob(b) => b })

  val `null`: Codec[None.type] =
    new PrimitiveCodec("NULL", _ => LiteValue.Null, { case LiteValue.Null => None })

  def unit: Codec[Unit] = new:
    def encode(u: Unit): List[LiteValue] = Nil
    def decode: StateT[Either[Throwable, *], List[LiteValue], Unit] = StateT.pure(())

  def nil: Codec[EmptyTuple] = unit.imap(_ => EmptyTuple)(_ => ())

  given InvariantMonoidal[Codec] = new:
    def unit = Codec.unit

    def product[A, B](fa: Codec[A], fb: Codec[B]) = new:
      def encode(ab: (A, B)) =
        val (a, b) = ab
        fa.encode(a) ::: fb.encode(b)

      def decode = fa.decode.product(fb.decode)

    def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A) = new:
      def encode(b: B) = fa.encode(g(b))
      def decode = fa.decode.map(f)
