// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.pfa.impl
import org.maraist.graphviz.
  {Graphable, GraphvizOptions, NodeLabeling, TransitionLabeling}
import org.maraist.fa.pfa.{PFA,PFAtraverser}
import org.maraist.fa.impl.{DOT}

/**
 * @group graphviz
 */
private[fa] class PFAdotTraverser[S, T, P >: PFA[S,T] <: Graphable[S,T]](
  val sb: StringBuilder,
  val nodeLabeling: NodeLabeling[S, T],
  val trLabeling: TransitionLabeling[T],
  val graphvizOptions: GraphvizOptions[S, T])
extends PFAtraverser[S, T, P]() {

  override def state(pfa: P, si: Int, s: S,
                     initialProb: Double, finalProb: Double): Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si)
    sb ++= " [shape="
    if (finalProb > 0.0) {
      sb ++= graphvizOptions.finalNodeShape
    } else {
      sb ++= graphvizOptions.nodeShape
    }
    sb ++= ",label=<<sup><font color=\"#0000ff\">"
    sb ++= si.toString()
    sb ++= "</font></sup>"
    sb ++= nodeLabeling.getLabel(s, pfa)
    sb ++= "; <font color=\"blue\">"
    sb ++= finalProb.toString()
    sb ++= DOT.endFontAndDot
    if (initialProb>0.0) {
      sb ++= "\tI"
      sb ++= si.toString()
      sb ++= " [shape=none, margin=0, label=\"\"]\n\tI"
      sb ++= si.toString()
      sb ++= DOT.graphvizArrowToVmark
      sb ++= si.toString()
      sb ++= " [ label=<<font color=\"blue\">"
      sb ++= initialProb.toString()
      sb ++= DOT.endFontAndDot
    }
  }
  override def presentEdge(
    pfa: P, si0: Int, s0: S, ti0: Int, t: T, si1: Int, s1: S, prob: Double):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<"
    sb ++= trLabeling.getLabel(t)
    sb ++= "; <font color=\"blue\">"
    sb ++= prob.toString()
    sb ++= DOT.endFontAndDot
    //println(si0 + "--[" + t + "]-->" + si1);
  }
  override def presentEdge(
    pfa: P, si0: Int, s0: S, si1: Int, s1: S, prob: Double):
      Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<&epsilon;; <font color=\"blue\">"
    sb ++= prob.toString()
    sb ++= DOT.endFontAndDot
    //println(si0 + "--[" + t + "]-->" + si1);
  }
}
