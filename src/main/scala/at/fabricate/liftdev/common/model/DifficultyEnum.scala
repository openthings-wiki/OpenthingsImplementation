package at.fabricate.liftdev.common
package model

import at.fabricate.liftdev.common.lib.EnumWithDescriptionAndObject
import scala.xml.Elem

	// add a difficulty (one of many that comes from a list of string options)
    object DifficultyEnum extends EnumWithDescriptionAndObject[Elem] {
  
    val difficultyText = "suitable for"
    val stage1 = "Kids"
    val stage2 = "Starter"
    val stage3 = "Average"
    val stage4 = "Advanced"
    val stage5 = "Expert"
    val stage6 = "Genius"
    
      
      private def wrapSpanWithClass(theClass : String,theText: String) : Elem = 
        <li class="left">
			<span class={theClass}></span>
			<h5>{difficultyText}</h5>
			<h6>{theText}</h6>
		</li>
//      <span class={theClass}></span>
    
	val kids = Value(stage1,wrapSpanWithClass("icon-difficulty1",stage1))
	val starter = Value(stage2,wrapSpanWithClass("icon-difficulty2",stage2))
	val average = Value(stage3,wrapSpanWithClass("icon-difficulty3",stage3))
	val advanced = Value(stage4,wrapSpanWithClass("icon-difficulty4",stage4))
	val expert = Value(stage5,wrapSpanWithClass("icon-difficulty5",stage5))
	val genius = Value(stage6,wrapSpanWithClass("icon-difficulty6",stage6))
	
	val upToKidsList = kids :: Nil
    val upToStarterList = starter  :: upToKidsList
    val upToAverageList = average  :: upToStarterList
    val upToAdvancedList = advanced   :: upToAverageList
    val upToExpertList = expert   :: upToAdvancedList
    val upToGeniusList = genius   :: upToExpertList
	}