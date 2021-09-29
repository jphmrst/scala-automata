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
import org.maraist.graphviz.{GraphvizOptions,NodeLabeling,TransitionLabeling}
import org.maraist.fa.impl.{DOT, DotTraverseMixin}
import org.maraist.fa.hyperedges.{HyperedgeDFAtraverser}
import org.maraist.fa.DFA.{DFAtraverser}

/**
  * @group graphviz
  */
private[fa] trait HyperedgeDOTmixin[S] {
  val sb:StringBuilder
  val stateList:IndexedSeq[S]

  private var edge:Int = 0
  def eHyperedge(fromStateI:Int, fromState:S, targetSet:Set[S]):Unit = {
    val nodeName:String = "EHE" + edge
    sb ++= "\t"
    sb ++= nodeName
    sb ++= " [shape=point, margin=0, label=\"\", color=\"gray\" ];\n"
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(fromStateI)
    sb ++= DOT.graphvizArrow
    sb ++= nodeName
    sb ++= " [ label=\"\", color=\"gray\", arrowhead=\"none\" ];\n"

    for (target <- targetSet) {
      val targetI = stateList.indexOf(target)
      sb ++= "\t"
      sb ++= nodeName
      sb ++= DOT.graphvizArrowToVmark
      sb ++= Integer.toString(targetI)
      sb ++= " [ label=\"\", color=\"gray\" ];\n"
    }

    edge += 1
  }
}

/**
  * @group graphviz
  */
private[fa] trait DOTQuietDFAMethods[S,T] {
  def init(states:Int, labels:Int): Unit = { }
  def absentEdge(fromIndex:Int, fromState:S, labelIndex:Int, label:T): Unit = { }
  def finish(): Unit = { }
}

/**
  * @group graphviz
  */
private[fa] class DotTraverseHyperedgeDFA[S, T](
  val graphvizOptions:GraphvizOptions,
  val sb:StringBuilder,
  val nodeLabeling:NodeLabeling[S, T],
  val trLabeling:TransitionLabeling[T],
  val stateList:IndexedSeq[S],
  val initialState:S)
extends HyperedgeDFAtraverser[S, T] with DotTraverseMixin[S, T]
with HyperedgeDOTmixin[S] with DOTQuietDFAMethods[S, T]
