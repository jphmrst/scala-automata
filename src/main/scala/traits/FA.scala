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

/** Core methods of any automaton with indexed storage.
  * @tparam S Type representing states.
  * @tparam T Type representing transition labels.
  * @tparam Z Type of style options for Graphviz export
  */
trait FA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends UnindexedFA[S, T, Z] {

  override def states: IndexedSeq[S]
  override def labels: IndexedSeq[T]

  /** Return the state at a particular index. */
  def state(i: Int): S

  /** Return the index of a state.  Since this is a [[Map]], a call on a
    * value which is not a state will throw an exception; use
    * getIndexOf for a safe version. */
  def indexOf: Map[S, Int]

  /** Return the index of a state, or None. */
  def getIndexOf(s: S): Option[Int]

  /** Return the indices of the initial states. */
  def initialStateIndices: Set[Int]

  /** Return the indices of the final states. */
  def finalStateIndices: Set[Int]

  /** Index of a particular label.  Since this is a [[Map]], a call on a
    * value which is not a label will throw an exception; use
    * getIndexOf for a safe version. */
  def labelIndex: Map[T, Int]

  /** Return the index of a label, or None. */
  def getLabelIndex(t: T): Option[Int]

  /** Label at a particular index. */
  def label(i:Int): T
}
