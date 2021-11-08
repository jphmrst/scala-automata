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
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

class EdgeAnnotatedNFA[S, T, NA, DA](
  override val stateSeq: IndexedSeq[S],
  override val initialStateIndices: Set[Int],
  override val finalStateIndices: Set[Int],
  override val transitionsSeq: IndexedSeq[T],
  override val transitionsArray: Array[Array[Set[Int]]],
  override val epsilonsArray: Array[Set[Int]],
  override val labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
  override val unlabelledEdgeAnnotations: Array[Array[Option[NA]]],
  override protected val combiner: EdgeAnnotationCombiner[NA, DA])

extends full.EdgeAnnotatedNFA[
  S, T, NA, DA, Set, EdgeAnnotatedDFA,
  EdgeAnnotatedAutomatonStyle, EdgeAnnotatedAutomatonStyle] {

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    initialAnnotation: Option[DA],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[DA]]]):
      EdgeAnnotatedDFA[Set[S], T, DA] =
    new EdgeAnnotatedDFA[Set[S], T, DA](
      dfaStates, initialStateIdx, dfaFinals, initialAnnotation,
      transitionsSeq, dfaTransitions, edgeAnnotations)

  def derivedNFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    transitionsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    finalStateIndices: Set[Int],
    initialStateIndices: Set[Int],
    labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
    unlabelledEdgeAnnotations: Array[Array[Option[NA]]]):
      EdgeAnnotatedNFA[S0, T0, NA, DA] =
    new EdgeAnnotatedNFA[S0, T0, NA, DA](
      stateSeq, initialStateIndices, finalStateIndices,
      transitionsSeq, transitionsArray, epsilonsArray,
      labelledEdgeAnnotations, unlabelledEdgeAnnotations,
      combiner)
}

object EdgeAnnotatedNFA {
  def newBuilder[S, T, NA, DA](combiner: EdgeAnnotationCombiner[NA, DA]) =
    new EdgeAnnotatedNFABuilder[S, T, NA, DA](combiner)
}
