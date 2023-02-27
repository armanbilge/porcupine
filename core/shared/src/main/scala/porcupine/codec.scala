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
import scodec.bits.ByteVector

trait Encoder[A]:
  outer =>

  def encode(a: A): List[LiteValue]

  def either[B](right: Encoder[B]): Encoder[Either[A, B]] = new:
    def encode(aorb: Either[A, B]) = aorb match
      case Left(a) => outer.encode(a)
      case Right(b) => right.encode(b)

  def opt: Encoder[Option[A]] =
    either(Codec.`null`).contramap(_.toLeft(None))

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
  outer =>

  def decode: StateT[Either[Throwable, *], List[LiteValue], A]

  def or[AA >: A](other: Decoder[AA]): Decoder[AA] = new:
    def decode = outer.decode.widen[AA].handleErrorWith(_ => other.decode)

  def either[B](right: Decoder[B]): Decoder[Either[A, B]] =
    outer.map(Left(_)).or(right.map(Right(_)))

  def opt: Decoder[Option[A]] =
    outer.map(Some(_)).or(Codec.`null`.asDecoder.as(None))

object Decoder:
  given Applicative[Decoder] = new:
    def pure[A](a: A) = new:
      def decode = StateT.pure(a)

    def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]) = new:
      def decode = ff.decode.ap(fa.decode)

    override def map[A, B](fa: Decoder[A])(f: A => B) = new:
      def decode = fa.decode.map(f)

trait Codec[A] extends Encoder[A], Decoder[A]:
  outer =>

  def asEncoder: Encoder[A] = this
  def asDecoder: Decoder[A] = this

  def either[B](right: Codec[B]): Codec[Either[A, B]] = new:
    def encode(aorb: Either[A, B]) =
      outer.asEncoder.either(right).encode(aorb)

    def decode = outer.asDecoder.either(right).decode

  override def opt: Codec[Option[A]] =
    either(Codec.`null`).imap(_.left.toOption)(_.toLeft(None))

object Codec:
  val integer: Codec[Long] = new:
    def encode(l: Long) = LiteValue.Integer(l) :: Nil
    def decode = StateT {
      case LiteValue.Integer(l) :: tail => Right((tail, l))
      case other => Left(new RuntimeException(s"Expected integer, got ${other.headOption}"))
    }

  val real: Codec[Double] = new:
    def encode(d: Double) = LiteValue.Real(d) :: Nil
    def decode = StateT {
      case LiteValue.Real(d) :: tail => Right((tail, d))
      case other => Left(new RuntimeException(s"Expected real, got ${other.headOption}"))
    }

  val text: Codec[String] = new:
    def encode(s: String) = LiteValue.Text(s) :: Nil
    def decode = StateT {
      case LiteValue.Text(s) :: tail => Right((tail, s))
      case other => Left(new RuntimeException(s"Expected text, got ${other.headOption}"))
    }

  val blob: Codec[ByteVector] = new:
    def encode(b: ByteVector) = LiteValue.Blob(b) :: Nil
    def decode = StateT {
      case LiteValue.Blob(b) :: tail => Right((tail, b))
      case other => Left(new RuntimeException(s"Expected blob, got ${other.headOption}"))
    }

  val `null`: Codec[None.type] = new:
    def encode(n: None.type) = LiteValue.Null :: Nil
    def decode = StateT {
      case LiteValue.Null :: tail => Right((tail, None))
      case other => Left(new RuntimeException(s"Expected NULL, got ${other.headOption}"))
    }

  def unit: Codec[Unit] = new:
    def encode(u: Unit) = Nil
    def decode = StateT {
      case Nil => Right((Nil, ()))
      case other => Left(new RuntimeException(s"Expected nothing, got ${other.headOption}"))
    }

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
