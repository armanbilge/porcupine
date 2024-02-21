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
import scodec.bits.ByteVector

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
          new AbstractDatabase[F]:
            def prepare[A, B](query: Query[A, B]): Resource[F, Statement[F, A, B]] =
              Resource
                .make {
                  mutex.lock.surround {
                    F.delay {
                      val zSql = query.sql.getBytes
                      val stmt = stackalloc[Ptr[sqlite3_stmt]]()
                      guard(db)(sqlite3_prepare_v2(db, zSql.at(0), zSql.length, stmt, null))
                      !stmt
                    }
                  }
                } { stmt =>
                  mutex.lock.surround(F.delay(guard(db)(sqlite3_finalize(stmt))))
                }
                .map { stmt =>
                  new AbstractStatement[F, A, B]:
                    def cursor(args: A): Resource[F, Cursor[F, B]] = mutex.lock *> Resource
                      .make {
                        F.delay {
                          var i = 1
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
                                sqlite3_bind_text(stmt, i, b.at(0), b.length, null),
                              )
                              i += 1
                              List(b)
                            case LiteValue.Blob(b) =>
                              val ba = b.toArray
                              guard(db)(
                                sqlite3_bind_blob64(stmt, i, ba.at(0), ba.length, null),
                              )
                              i += 1
                              List(ba)
                          }
                        }
                      }(x => F.delay(x).void) // to keep in sight of gc
                      .as { maxRows =>
                        F.blocking {
                          val rows = List.newBuilder[List[LiteValue]]
                          var i = 0
                          var continue = true
                          while i < maxRows && continue do
                            sqlite3_step(stmt) match
                              case SQLITE_ROW =>
                                rows += List.tabulate(sqlite3_column_count(stmt)) { j =>
                                  sqlite3_column_type(stmt, j) match
                                    case SQLITE_NULL =>
                                      LiteValue.Null
                                    case SQLITE_INTEGER =>
                                      LiteValue.Integer(sqlite3_column_int64(stmt, j))
                                    case SQLITE_FLOAT =>
                                      LiteValue.Real(sqlite3_column_double(stmt, j))
                                    case SQLITE_TEXT =>
                                      LiteValue.Text(fromCString(sqlite3_column_text(stmt, j)))
                                    case SQLITE_BLOB =>
                                      LiteValue.Blob(
                                        ByteVector.fromPtr(
                                          sqlite3_column_blob(stmt, j),
                                          sqlite3_column_bytes(stmt, j),
                                        ),
                                      )
                                }
                              case SQLITE_DONE => continue = false
                              case other => guard(db)(other)

                            i += 1
                          (rows.result(), continue)
                        }.flatMap { (rows, continue) =>
                          rows
                            .traverse(query.decoder.decode.runA(_).liftTo)
                            .tupleRight(continue)
                        }
                      }
                }
        }
    }

  inline private def guard(thunk: => CInt): Unit =
    val rtn = thunk
    if rtn != SQLITE_OK then throw new RuntimeException(fromCString(sqlite3_errstr(rtn)))

  inline private def guard(db: Ptr[sqlite3])(thunk: => CInt): Unit =
    if thunk != SQLITE_OK then throw new RuntimeException(fromCString(sqlite3_errmsg(db)))
