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
import scala.collection.mutable.{Builder}
import org.maraist.fa.traits

case class AddState[S, T](state: S)
case class RemoveState[S, T](state: S)
case class RemoveFinalState[S, T](state: S)
case class AddFinalState[S, T](state: S)

/** All [[scala.collection.mutable.Builder Builder]]-pattern elements
  * pertaining to any [[traits.FA finite automaton]].
  *
  *  @tparam S The type of all states of the automaton
  *  @tparam T The type of labels on transitions of the automaton
  *
  * @group builderElements
  */
type FAelements[S, T] = (
  AddState[S, T]
    | RemoveState[S, T]
    | AddFinalState[S, T]
    | RemoveFinalState[S, T]
)
