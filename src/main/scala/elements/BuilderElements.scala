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
import scala.collection.mutable.{Builder, HashMap, HashSet}
import org.maraist.fa.DFA.IndexedDFA

case class AddState[S,T](state: S)
case class RemoveState[S,T](state: S)
case class RemoveFinalState[S,T](state: S)
case class AddFinalState[S,T](state: S)

/** [[scala.collection.mutable.Builder Builder]]-pattern element for
  * setting the initial state in a [[DFABuilder DFA builder]].
  * @tparam S The type of all states of the automaton
  * @group builderElements
  */
case class SetInitialState[S](state: S)
case class AddInitialState[S](state: S)
case class RemoveInitialState[S](state: S)

case class AddTransition[S,T](state1: S, trans: T, state2: S)
case class RemoveTransition[S,T](state1: S, trans: T, state2: S)

case class AddETransition[S](state1: S, state2: S)
case class RemoveETransition[S](state1: S, state2: S)
