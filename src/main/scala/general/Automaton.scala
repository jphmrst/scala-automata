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
import org.maraist.fa.traits.
  {StateHolder, FinalStateSetHolder, InitialStateSetHolder, LabelsHolder}

/** Trait of the basic usage operations on an automaton with states and labels.
 *
 *  Makes no assumptions about the nature of the transitions between states.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group General
 */
trait Automaton[S,T]
    extends StateHolder[S]
    with FinalStateSetHolder[S]
    with InitialStateSetHolder[S]
    with LabelsHolder[T]

