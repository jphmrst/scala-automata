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

/** Methods associated with a deterministic finite automaton.
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam Z Type of style options for Graphviz export
  */
trait UnindexedDFA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends UnindexedFA[S, T, Z] {

  /** Returns the initial state. */
  def initialState: S

  /** Returns the state, if any, into which the automaton could
    * transition starting from `s` via a transition labelled `t`.
    */
  def transition(s: S, t: T): Option[S]
}
