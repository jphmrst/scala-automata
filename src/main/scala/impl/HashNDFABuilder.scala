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
import scala.collection.mutable.{Builder,HashSet}
import org.maraist.fa.elements.*
import org.maraist.fa.NDFA
import org.maraist.fa.NDFA.*

/**
  *
  *  @group NDFA
  */
class HashNDFABuilder[S,T]
    extends AbstractHashNDFABuilder[S,T,ArrayDFA[Set[S],T],ArrayNDFA[S,T], NDFAelements[S,T]]
{

  def toDFA: ArrayDFA[Set[S],T] = toNDFA.toDFA
  protected def assembleNDFA(
    statesSeq: IndexedSeq[S],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[HashSet[Int]]],
    epsilonsArray: Array[HashSet[Int]]
  ): ArrayNDFA[S,T] =
    new ArrayNDFA[S,T](
      statesSeq, initials, finals, transitionsSeq,
      labelsArray.map(_.map(_.toSet)),
      epsilonsArray.map(_.toSet))
}
