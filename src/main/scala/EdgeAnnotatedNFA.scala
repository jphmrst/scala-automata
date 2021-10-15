// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import org.maraist.fa.full
import org.maraist.fa.util.{EdgeAnnotationCombiner,IndexSetsTracker}
import org.maraist.fa.styles.AutomatonStyle

class EdgeAnnotatedNFA[S, T, NA, DA](
  override val stateSeq: IndexedSeq[S],
  override val initialStateIndices: Set[Int],
  override val finalStateIndices: Set[Int],
  override val transitionsSeq: IndexedSeq[T],
  override val transitionsArray: Array[Array[Set[Int]]],
  override val epsilonsArray: Array[Set[Int]],
  override val labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
  override val unlabelledEdgeAnnotations: Array[Array[Option[NA]]])(
  using combiner: EdgeAnnotationCombiner[NA, DA])

extends full.EdgeAnnotatedNFA[
  S, T, NA, DA, Set, EdgeAnnotatedDFA, AutomatonStyle] {

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[DA]]]):
      EdgeAnnotatedDFA[Set[S], T, DA] = new EdgeAnnotatedDFA[Set[S], T, DA](
        dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
        dfaTransitions, edgeAnnotations)
}
