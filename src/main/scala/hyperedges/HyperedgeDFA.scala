// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.hyperedges
import org.maraist.graphviz.{GraphvizOptions,NodeLabeling,TransitionLabeling}
import org.maraist.fa.elements.HasBuilderWithInit
import org.maraist.fa.DFA
import org.maraist.fa.DFA.IndexedDFA
import org.maraist.fa.hyperedges.Builders.HyperedgeDFAelements

/**
  *  @group Hyperedge
  */
trait HyperedgeDFA[S,T] extends DFA[S,T] with Hyperedge[S, T] {
  override type Traverser <: HyperedgeDFAtraverser[S,T]

  /** {@inheritDoc} */
  override protected def internalsToDOT(
    stateList:IndexedSeq[S], sb:StringBuilder
  )(using
    nodeLabeling: NodeLabeling[S, T],
    transitionLabeling: TransitionLabeling[T],
    graphvizOptions: GraphvizOptions
  ):
      Unit = {
    super.internalsToDOT(stateList, sb)
    // eHyperedgesToDOT(nodeLabeling, stateList, sb)
  }

  override def traverseEdgesFrom(si0:Int, s0:S, stateList:IndexedSeq[S],
                                 theLabels:IndexedSeq[T],
                                 trav:Traverser):Unit = {
    super.traverseEdgesFrom(si0, s0, stateList, theLabels, trav)
    for (targets <- eHyperedgeTargets(s0)) {
      trav.eHyperedge(si0, s0, targets)
    }
  }
}

/**
  *  @group Hyperedge
  */
object HyperedgeDFA {
  def newBuilder[S, T](initialState: S)
    (using impl: HasBuilderWithInit[HyperedgeDFAelements, ?, HyperedgeDFA]) =
    impl.build[S,T](initialState)
}

/**
  *  @group Hyperedge
  */
trait IndexedHyperedgeDFA[S,T] extends IndexedDFA[S,T] with HyperedgeDFA[S,T]

/**
 * Methods for traversing the structure of a
 * {@link org.maraist.fa.HyperedgeDFA HyperedgeDFA}. Use with the
 * {@link org.maraist.fa.HyperedgeDFA#traverse HyperedgeDFA.traverse} method.
 * By default, all methods are empty.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group DFA
 */
trait HyperedgeDFAtraverser[S,-T] extends DFA.DFAtraverser[S,T] {
  def eHyperedge(sourceIndex:Int, source:S, targets:Set[S]): Unit
}
