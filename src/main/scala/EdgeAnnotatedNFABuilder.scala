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
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.elements.EdgeAnnotatedNFAelements
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.fa.full

class EdgeAnnotatedNFABuilder[S, T, NA, DA](
  using EdgeAnnotationCombiner[NA, DA])

extends full.EdgeAnnotatedNFABuilder[
  S, T, NA, DA, Set, EdgeAnnotatedDFA, EdgeAnnotatedNFA,
  EdgeAnnotatedNFAelements[S, T, NA],
  EdgeAnnotatedAutomatonStyle, EdgeAnnotatedAutomatonStyle
]

with EdgeAnnotatedNFABuilder.Completer[S, T, NA, DA]


object EdgeAnnotatedNFABuilder {
  /** Mixin providing a definition of [[#assembleNFA]] for the standard
    * top-level classes of this builder.
    */
  trait Completer[S, T, NA, DA](
    using combiner: EdgeAnnotationCombiner[NA, DA]) {

    protected def assembleNFA(
      statesSeq: IndexedSeq[S],
      initials: Set[Int],
      finals: Set[Int],
      transitionsSeq: IndexedSeq[T],
      labelsArray: Array[Array[Set[Int]]],
      epsilonsArray: Array[Set[Int]],
      labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
      unlabelledEdgeAnnotations: Array[Array[Option[NA]]]):
        EdgeAnnotatedNFA[S, T, NA, DA] =
      new EdgeAnnotatedNFA[S, T, NA, DA](
        statesSeq, initials, finals, transitionsSeq, labelsArray, epsilonsArray,
        labelledEdgeAnnotations, unlabelledEdgeAnnotations
      )
  }
}
