// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.hyperedges.impl
import scala.collection.mutable.{Builder,HashSet,HashMap}
import org.maraist.graphviz.
  {GraphvizOptions, NodeLabeling, TransitionLabeling}
import org.maraist.fa.DFABuilder
import org.maraist.fa.hyperedges.{
  HyperedgeDFAtraverser, HyperedgeDFA, HyperedgeDFABuilder}
import org.maraist.fa.elements.*
import org.maraist.fa.DFA.*
import org.maraist.fa.hyperedges.Builders.*
import org.maraist.fa.impl.{AbstractHashDFABuilder}

/**
  *
  * @group Hyperedge
  */
class HashHyperedgeDFABuilder[S, T](initialState: S)
    extends AbstractHashDFABuilder[S, T, ArrayHyperedgeDFA[S, T],HyperedgeDFAelements[S,T]](initialState)
    with HyperedgeDFABuilder[S, T, ArrayHyperedgeDFA[S, T], HyperedgeDFAelements[S,T]]
    with Builder[HyperedgeDFAelements[S, T], HyperedgeDFA[S, T]]{
  val hyperedgeMap: HashMap[S,HashSet[Set[S]]] = new HashMap[S,HashSet[Set[S]]]
  def eHyperedgeTargets(s:S): Set[Set[S]] = hyperedgeMap.get(s) match {
    case Some(set) => set.toSet
    case None => Set.empty[Set[S]]
  }
  def addEHyperedge(s:S, ss:Set[S]): Unit = {
    if (!hyperedgeMap.contains(s))  hyperedgeMap += (s -> new HashSet[Set[S]])
    hyperedgeMap(s) += ss
  }

  type Traverser = HyperedgeDFAtraverser[S,T, this.type]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) =
    new DotTraverseHyperedgeDFA[S,T, this.type](
      summon[GraphvizOptions[S, T]], sb, summon[NodeLabeling[S, T]],
      summon[TransitionLabeling[T]], stateList, initialState)

  protected def assembleDFA(statesSeq: IndexedSeq[S],
                            initialIdx: Int,
                            finalStateIndices: HashSet[Int],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Int]]): ArrayHyperedgeDFA[S,T] = {
    val hyperedgeIndicesMap = new HashMap[Int,HashSet[Set[Int]]]
    for((node,nodeSets) <- hyperedgeMap) {
      val nodeIdx = statesSeq.indexOf(node)
      val thisSet = new HashSet[Set[Int]]
      for(nodeSet <- nodeSets)
        thisSet += nodeSet.map(statesSeq.indexOf(_))
      hyperedgeIndicesMap(nodeIdx) = thisSet
    }
    new ArrayHyperedgeDFA[S,T](
      statesSeq, initialIdx, finalStateIndices.toSet,
      transitionsSeq, idxLabels,
      hyperedgeIndicesMap.map({case (k, v) => (k, v.toSet)}).toMap)
  }

  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  protected override def addBuilderElement(builder: HyperedgeDFAelements[S,T]):
      Unit =
    builder match {
      case AddEHyperedge(state, toStates) => addEHyperedge(state, toStates)
      case _: DFAelements[S, T] => super.addBuilderElement(builder)
    }
}
