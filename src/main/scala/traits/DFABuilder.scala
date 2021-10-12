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
import org.maraist.fa.elements
import org.maraist.fa.styles.AutomatonStyle

/** Methods for builders of a deterministic finite automaton.
  *
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam D Type of DFA constructed by this builder.
  * @tparam K Builder elements for this builder.
  * @tparam Z Type of style options for Graphviz export
  */
trait DFABuilder[
  S, T,
  +D[DS, DT] <: DFA[DS, DT, Z],
  -K[KS, KT] >: elements.DFAelements[KS, KT] <: Matchable,
  -Z[S, T] <: AutomatonStyle[S, T]]

extends FABuilder[S, T, D, K, Z] with UnindexedDFA[S, T, Z] {

  /** Removes any transition labelled `t` from `s1`. */
  def removeTransition(s1: S, t: T): Unit

  /** Set the initial state of the DFA. */
  def setInitialState(s: S): Unit

//  /** Primary {@link scala.collection.mutable.Builder Builder} method
//    * implementation.
//    */
//  // final
//  def addOne(builder: K[S, T]): this.type
//  //   = {
//  //   addBuilderElement(builder)
//  //   this
//  // }
}
