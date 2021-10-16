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
import scala.collection.mutable.{Builder,HashSet}
import org.maraist.fa.elements.*
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.full

/**
  *
  *  @group NDFA
  */
class NFABuilder[S,T]

extends full.NFABuilder[
  S, T, Set, DFA, NFA, NFAelements[S,T], AutomatonStyle, AutomatonStyle] {

  override protected def assembleNFA(
    statesSeq: IndexedSeq[S],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]]
  ): NFA[S, T] =
    new NFA[S, T](
      statesSeq, transitionsSeq, initials, finals,
      epsilonsArray.map(_.toSet),
      labelsArray.map(_.map(_.toSet)))
}
