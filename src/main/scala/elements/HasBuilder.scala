// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.elements
import scala.collection.mutable.Builder


trait HasBuilder[
  Elements[_,_],
  B[X,Y] <: Builder[Elements[X,Y], ? <: Res[X,Y]],
  Res[_,_]
] {
  def build[S,T](): B[S,T]
}

trait HasBuilderWithInit[
  Elements[_,_],
  B[X,Y] <: Builder[Elements[X,Y], ? <: Res[X,Y]],
  Res[_,_]
] {
  def build[S,T](init: S): B[S,T]
}
