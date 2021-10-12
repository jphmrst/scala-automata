// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.traits
import org.maraist.fa.styles.AutomatonStyle

/** Methods for builders of a deterministic finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam D Type of DFA constructed by this builder.
  * @tparam Z Type of style options for Graphviz export
  */
trait DFABuilder[
  S, T,
  +D[DS, DT, -DZ[DS, DT] <: Z[DS, DT]] <: DFA[S, T, Z],
  -Z[S, T] <: AutomatonStyle[S, T]]

extends FABuilder[S, T, Z] with UnindexedDFA[S, T, Z] {

  /** Removes any transition labelled `t` from `s1`. */
  def removeTransition(s1: S, t: T): Unit

  /** Return the specified DFA. */
  def toDFA: D[S, T, Z]

  /** Set the initial state of the DFA. */
  def setInitialState(s: S): Unit
}
