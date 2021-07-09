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
import org.maraist.graphviz.{GraphvizOptions,NodeLabeling,TransitionLabeling}
import org.maraist.fa.DFA.{DFAtraverser}

/**
  * @group graphviz
  */
private[fa] object DOT {
  val tabToVmark:String = "\tV"
  val graphvizArrow:String = " -> "
  val graphvizArrowToVmark:String = " -> V"
  val endFontAndDot:String = "</font>> ];\n"
}

/**
  * @group graphviz
  */
private[fa] trait DotTraverseMixin[S,T] {
  val graphvizOptions:GraphvizOptions
  val sb:StringBuilder
  val nodeLabeling:NodeLabeling[S]
  val trLabeling:TransitionLabeling[T]
  val stateList:IndexedSeq[S]
  val initialState:S

  def state(si:Int, s:S, isInitial:Boolean, isFinal:Boolean): Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si)
    sb ++= " [shape="
    if (isFinal) {
      sb ++= graphvizOptions.finalNodeShape
    } else {
      sb ++= graphvizOptions.nodeShape
    }
    sb ++= ",label=<<sup><font color=\"#0000ff\">"
    sb ++= si.toString()
    sb ++= "</font></sup>"
    sb ++= nodeLabeling.getLabel(s)
    sb ++= ">]\n"
  }
  def postState():Unit = {
    // Arrow for the initial state
    sb ++= "\tinit -> V"
    sb ++= Integer.toString(stateList.indexOf(initialState))
    sb ++= ";\n"
  }
  def presentEdge(si0:Int, s0:S, ti0:Int, t:T, si1:Int, s1:S):Unit = {
    sb ++= DOT.tabToVmark
    sb ++= Integer.toString(si0)
    sb ++= DOT.graphvizArrowToVmark
    sb ++= Integer.toString(si1)
    sb ++= " [ label=<"
    sb ++= trLabeling.getLabel(t)
    sb ++= "> ];\n"
    //println(si0 + "--[" + t + "]-->" + si1);
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
private[fa] class DotTraverseDFA[S,T](val graphvizOptions:GraphvizOptions,
                                      val sb:StringBuilder,
                                      val nodeLabeling:NodeLabeling[S],
                                      val trLabeling:TransitionLabeling[T],
                                      val stateList:IndexedSeq[S],
                                      val initialState:S)
extends DFAtraverser[S,T]
with DotTraverseMixin[S,T] with DOTQuietDFAMethods[S,T]
