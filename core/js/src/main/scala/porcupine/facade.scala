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
package sqlite3

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("sqlite3")
private[porcupine] class Database(filename: String, cb: js.Function1[js.Error, Unit])
    extends js.Object:

  def close(cb: js.Function1[js.Error, Unit]): Unit = js.native

  def prepare(sql: String, cb: js.Function1[js.Error, Unit]): Statement = js.native

  def interrupt(): Unit = js.native

@js.native
private[porcupine] trait Statement extends js.Object:

  def bind(params: Any*): Statement = js.native

  def reset(cb: js.Function0[Unit]): Statement = js.native

  def finalize(cb: js.Function0[Unit]): Unit = js.native

  def get(cb: js.Function2[js.Error, js.Array[Any], Unit]): Statement = js.native
