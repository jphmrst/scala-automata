// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.general
import org.maraist.graphviz.Graphable
import org.maraist.graphviz.NodeLabeling
import org.maraist.graphviz.TransitionLabeling
import org.maraist.fa.traits.
  {IndexedStateHolder, IndexedLabelsHolder, IndexedInitialStateSetHolder,
    IndexedFinalStateSetHolder}

/** Trait of the basic usage operations of an automaton whose states and
 * transition labels can be referenced by an index number.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
  *  @group General
 */
trait IndexedAutomaton[S,T]
    extends Automaton[S,T]
    with IndexedStateHolder[S]
    with IndexedLabelsHolder[T]
    with IndexedInitialStateSetHolder[S]
    with IndexedFinalStateSetHolder[S]
