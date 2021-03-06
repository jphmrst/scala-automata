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
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

/** Implementation of a edge-annotated DFA.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
trait UnindexedEdgeAnnotatedDFA[
  S, T, A,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]]

extends UnindexedEdgeAnnotatedFA[S, T, A, Z]

with UnindexedDFA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]] {

  /** Return any annotation associated "before" the initial state.
    */
  def initialAnnotation: Option[A]

  /** Return whether there is an annotation associated "before" the
    * initial state.
    */
  def initialAnnotated: Boolean

  /** Return the annotation (if any) on the transition from the given
    * state and with the given label.
    */
  def annotation(src: S, label: T): Option[A]
}
