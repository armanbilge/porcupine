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

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.syntax.all.*

import scala.scalanative.unsafe.*

import sqlite3.*

private abstract class DatabasePlatform:
  def open[F[_]](filename: String)(using F: Async[F]): Resource[F, Database[F]] =
    Resource.eval(Mutex[F]).flatMap { mutex =>
      Resource
        .make {
          F.blocking {
            val fn = (filename + 0.toChar).getBytes
            val db = stackalloc[Ptr[sqlite3]]()
            val flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_NOMUTEX

            try guard(sqlite3_open_v2(fn.at(0), db, flags, null))
            catch
              case t: Throwable =>
                if (!db ne null) guard(sqlite3_close(!db))
                throw t

            !db
          }
        }(db => F.blocking(guard(sqlite3_close(db))))
        .map { db =>
          new:
            def prepare[A, B](query: Query[A, B]): Resource[F, Statement[F, A, B]] =
              Resource
                .make {
                  F.delay {
                    val zSql = query.sql.getBytes
                    val stmt = stackalloc[Ptr[sqlite3_stmt]]()
                    guard(db)(sqlite3_prepare_v2(db, zSql.at(0), zSql.length, stmt, null))
                    !stmt
                  }
                } { stmt =>
                  F.delay(guard(db)(sqlite3_finalize(stmt)))
                }
                .map { stmt => args =>
                  Resource
                    .make {
                      F.delay {
                        var i = 0
                        query.encoder.encode(args).flatMap {
                          case LiteValue.Null =>
                            guard(db)(sqlite3_bind_null(stmt, i))
                            i += 1
                            Nil
                          case LiteValue.Integer(j) =>
                            guard(db)(sqlite3_bind_int64(stmt, i, j))
                            i += 1
                            Nil
                          case LiteValue.Real(d) =>
                            guard(db)(sqlite3_bind_double(stmt, i, d))
                            i += 1
                            Nil
                          case LiteValue.Text(s) =>
                            val b = s.getBytes
                            guard(db)(
                              sqlite3_bind_text(stmt, i, b.at(0), b.length, SQLITE_STATIC),
                            )
                            i += 1
                            List(b)
                          case LiteValue.Blob(b) =>
                            val ba = b.toArray
                            guard(db)(
                              sqlite3_bind_blob64(stmt, i, ba.at(0), ba.length, SQLITE_STATIC),
                            )
                            i += 1
                            List(ba)
                        }
                      }
                    }(x => F.delay(x).void) // to keep in sight of gc
                    .as { maxRows =>
                      ???
                    }
                }
        }
    }

  inline private def guard(thunk: => CInt): Unit =
    val rtn = thunk
    if rtn != SQLITE_OK then throw new RuntimeException(fromCString(sqlite3_errstr(rtn)))

  inline private def guard(db: Ptr[sqlite3])(thunk: => CInt): Unit =
    if thunk != SQLITE_OK then throw new RuntimeException(fromCString(sqlite3_errmsg(db)))
