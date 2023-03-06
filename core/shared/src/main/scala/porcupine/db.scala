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

import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Chunk
import fs2.Pipe
import fs2.Stream

import java.util.NoSuchElementException
import scala.annotation.targetName

abstract class Database[F[_]] private[porcupine]:
  def prepare[A, B](query: Query[A, B]): Resource[F, Statement[F, A, B]]

  final def cursor[A, B](query: Query[A, B], args: A): Resource[F, Cursor[F, B]] =
    prepare(query).flatMap(_.cursor(args))

  def execute[A, B](query: Query[A, B], args: A): F[List[B]]

  final def execute[A](query: Query[Unit, A]): F[List[A]] = execute(query, ())

  @targetName("executeVoid")
  def execute[A](query: Query[A, Unit], args: A): F[Unit]

  @targetName("executeVoid")
  final def execute(query: Query[Unit, Unit]): F[Unit] = execute(query, ())

  def option[A, B](query: Query[A, B], args: A): F[Option[B]]

  final def option[A](query: Query[Unit, A]): F[Option[A]] = option(query, ())

  def unique[A, B](query: Query[A, B], args: A): F[B]

  final def unique[A](query: Query[Unit, A]): F[A] = unique(query, ())

  def stream[A, B](query: Query[A, B], args: A, chunkSize: Int): Stream[F, B]

  def pipe[A, B](query: Query[A, B], chunkSize: Int): Pipe[F, A, B]

  final def pipe[A](query: Query[A, Unit], args: A): Pipe[F, A, Nothing] =
    in => pipe(query, 1)(in).drain

private abstract class AbstractDatabase[F[_]](using F: MonadCancelThrow[F]) extends Database[F]:

  final def execute[A, B](query: Query[A, B], args: A) = prepare(query).use(_.execute(args))

  @targetName("executeVoid")
  final def execute[A](query: Query[A, Unit], args: A) = prepare(query).use(_.execute(args))

  final def option[A, B](query: Query[A, B], args: A) = prepare(query).use(_.option(args))

  final def unique[A, B](query: Query[A, B], args: A) = prepare(query).use(_.unique(args))

  final def stream[A, B](query: Query[A, B], args: A, chunkSize: Int) =
    Stream.resource(prepare(query)).flatMap(_.stream(args, chunkSize))

  final def pipe[A, B](query: Query[A, B], chunkSize: Int) =
    in => Stream.resource(prepare(query)).flatMap(_.pipe(chunkSize)(in))

object Database extends DatabasePlatform

abstract class Statement[F[_], A, B] private[porcupine]:
  def cursor(args: A): Resource[F, Cursor[F, B]]

  def execute(args: A): F[List[B]]

  final def execute(using ev: Unit <:< A): F[List[B]] = execute(())

  def execute(using Unit <:< B)(args: A): F[Unit]

  final def execute(using Unit <:< A, Unit <:< B): F[Unit] = execute(())

  def option(args: A): F[Option[B]]

  final def option(using Unit <:< A): F[Option[B]] = option(())

  def unique(args: A): F[B]

  final def unique(using Unit <:< A): F[B] = unique(())

  def stream(args: A, chunkSize: Int): Stream[F, B]

  final def pipe(chunkSize: Int): Pipe[F, A, B] =
    _.flatMap(stream(_, chunkSize))

  final def pipe(using Unit <:< B): Pipe[F, A, Nothing] =
    in => pipe(1)(in).drain

private abstract class AbstractStatement[F[_], A, B](using F: MonadCancelThrow[F])
    extends Statement[F, A, B]:

  final def execute(args: A) = cursor(args).use(_.fetch(Int.MaxValue).map(_._1))

  final def execute(using Unit <:< B)(args: A) = cursor(args).use(_.fetch(1).void)

  final def option(args: A) = cursor(args).use(_.fetch(2).flatMap {
    case (Nil, _) => F.pure(None)
    case (head :: Nil, _) => F.pure(Some(head))
    case _ => F.raiseError(new RuntimeException("More than 1 row"))
  })

  final def unique(args: A) = cursor(args).use(_.fetch(2).flatMap {
    case (Nil, _) => F.raiseError(new NoSuchElementException)
    case (head :: Nil, _) => F.pure(head)
    case _ => F.raiseError(new RuntimeException("More than 1 row"))
  })

  final def stream(args: A, chunkSize: Int) =
    Stream.resource(cursor(args)).flatMap { cursor =>
      Stream
        .unfoldLoopEval(()) { _ =>
          cursor.fetch(chunkSize).map { (chunk, more) =>
            (Chunk.seq(chunk), Option.when(more)(()))
          }
        }
        .unchunks
    }

abstract class Cursor[F[_], A] private[porcupine]:
  def fetch(maxRows: Int): F[(List[A], Boolean)]
