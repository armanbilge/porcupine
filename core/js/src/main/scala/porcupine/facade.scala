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

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@nowarn
@js.native
@JSImport("better-sqlite3", JSImport.Default)
private[porcupine] class Database(filename: String) extends js.Object:

  def prepare(sql: String): Statement = js.native

  def close(): Database = js.native

  def defaultSafeIntegers(toggleState: Boolean): Database = js.native

@js.native
private[porcupine] trait Statement extends js.Object:

  def reader: Boolean = js.native

  def raw(toggleState: Boolean): Statement = js.native

  def iterate(bindParameters: js.Dictionary[Any]): js.Iterator[js.UndefOr[js.Array[Any]]] =
    js.native

  def run(bindParameters: js.Dictionary[Any]): js.Object = js.native
