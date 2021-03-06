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
  * @tparam D Type of DFA constructed by this NFA.
  * @tparam G Collection type constructor for states in constructed
  * DFAs.
  * @tparam Z Type of style options for Graphviz export
  */
trait UnindexedNFA[
  S, T,
  G[X] <: Set[X],
  +D[DS, DT] <: DFA[DS, DT, DZ],
  -NZ[S, T] <: AutomatonStyle[S, T],
  -DZ[S, T] <: AutomatonStyle[S, T]]

extends UnindexedFA[S, T, NZ] {

  /** Return the set of states at the end of an epsilon transition from
    * `s`. */
  def eTransitions(s: S): Set[S]

  /** Perform some action for each epsilon transition in the
    * automaton. */
  def foreachETransition(action: (s1: S, s2: S) => Unit): Unit

  /** Return the specified DFA. */
  def toDFA: D[G[S], T]
}
