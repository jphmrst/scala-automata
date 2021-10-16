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

case class AddTransition[S, T](state1: S, trans: T, state2: S)
case class RemoveTransition[S, T](state1: S, trans: T, state2: S)

case class AddETransition[S](state1: S, state2: S)
case class RemoveETransition[S](state1: S, state2: S)

/** All [[scala.collection.mutable.Builder Builder]]-pattern elements
  * pertaining to [[traits.NFA]]s.
  *
  *  @tparam S The type of all states of the automaton
  *  @tparam T The type of labels on transitions of the automaton
  *
  * @group builderElements
  */
type NFAelements[S, T] = (
  FAelements[S, T]
    | AddInitialState[S] | RemoveInitialState[S]
    | AddTransition[S, T] | RemoveTransition[S, T]
    | AddETransition[S] | RemoveETransition[S]
)
