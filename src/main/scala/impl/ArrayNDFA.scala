// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.impl
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import org.maraist.util.IndexSetsTracker

/**
 * Concrete implementation of an {@link org.maraist.fa.NDFA NDFA} based on
 * arrays.
 *
 * NDFAs of this type are translated to
 * {@link org.maraist.fa.ArrayDFA ArrayDFAs}.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group NDFA
 */
class ArrayNDFA[S,T](
  stateSeq: IndexedSeq[S],
  initialStateSet: Set[Int],
  finalStateSet: Set[Int],
  transitionsSeq: IndexedSeq[T],
  labels: Array[? <: Array[? <: Set[Int]]],
  epsilons: Array[? <: Set[Int]]
) extends AbstractArrayNDFA[S,T,ArrayDFA[Set[S],T]](
  stateSeq, initialStateSet, finalStateSet, transitionsSeq, labels, epsilons
) {

  protected def assembleDFA(dfaStates:IndexedSeq[Set[S]],
                            initialStateIdx:Int,
                            dfaFinals:Set[Int],
                            transitionsSeq:IndexedSeq[T],
                            dfaTransitions:Array[Array[Int]],
                            tracker:IndexSetsTracker,
                            appearsIn:Array[Set[Int]]): ArrayDFA[Set[S],T] = {
    //println(" ** assembling DFA: initial " + initialStateIdx + ", final states " + dfaFinals)
    val result:ArrayDFA[Set[S],T] =
      new ArrayDFA[Set[S],T](dfaStates, initialStateIdx, dfaFinals,
                             transitionsSeq, dfaTransitions)
    result
  }
}
