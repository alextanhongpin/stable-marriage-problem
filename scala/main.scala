type Score = Map[String, Int]
type Names = List[String]
type ScoreTable = Map[String, Score]
type RelationshipState = Map[String, String]
type Preferences = Map[String, List[String]]

// Make states explicit by using case classes
trait Status
case class NoRecord() extends Status
case class Single() extends Status
case class Engaged(name: String) extends Status
case class Married(male: String, female: String) extends Status

val males: Preferences = Map (
    "0" -> List("7", "5", "6", "4"),
    "1" -> List("5", "4", "6", "7"),
    "2" -> List("4", "5", "6", "7"),
    "3" -> List("4", "5", "6", "7"))

val females: Preferences = Map (
    "4" -> List("0", "1", "2", "3"),
    "5" -> List("0", "1", "2", "3"),
    "6" -> List("0", "1", "2", "3"),
    "7" -> List("0", "1", "2", "3"))

def makeScoreTable(females: Preferences): ScoreTable = {
  females.map(i => (i._1 -> i._2.indices.map(j => (i._2(j) -> j)).toMap)).toMap
}

def lookupScore(scores: ScoreTable, female: String, male: String): Int = {
  val maleScores: Score = scores.getOrElse(female, Map[String, Int]())
  maleScores.getOrElse(male, Int.MaxValue)
}

def checkStatus(name: String, state: RelationshipState): Status = state get name match {
  case None => NoRecord()
  case Some("") => Single()
  case Some(name) => Engaged(name)
}

def resetStatus(name: String, state: RelationshipState): RelationshipState = {
  val single: String = ""
  state + (name -> single)
}

def propose(male: String, female: String, state: RelationshipState): RelationshipState = {
  state + (male -> female, female -> male)
}

def makeMatches(matches: List[String], state: RelationshipState, preferences: Preferences): (RelationshipState, Preferences) = {
  def loop(items: List[String], state: RelationshipState, preferences: Preferences): (RelationshipState, Preferences) = items match {
    case Nil => (state, preferences)
    case (male :: Nil) => {
      checkStatus(male, state) match {
        case NoRecord() => {
          val newState = resetStatus(male, state)
          (newState, preferences)
        }

        case Single() => {
          val checkRemainingMalePreferences = preferences get male
          checkRemainingMalePreferences match {
            case None | Some(Nil) => (state, preferences)
            case Some(female :: _tail) => {
              val newPreferences = preferences + (male -> _tail)

              checkStatus(female, state) match {
                case NoRecord() | Single() => {
                  val newState = propose(male, female, state)
                  (newState, newPreferences)
                }
                case Engaged(currMale) => {
                  // WARNING: global state
                  val score1: Int = lookupScore(scoreTable, female, male)
                  val score2: Int = lookupScore(scoreTable, female, currMale)
                  if (score1 < score2) {
                    // Change partner
                    val breakupState = resetStatus(currMale, state)
                    val proposedState = propose(male, female, breakupState)
                    (proposedState, newPreferences)
                  } else {
                    (state, newPreferences)
                  }                  
                }
              }
            }
          }
        }
        // Already engaged to someone
        case Engaged(_) => (state, preferences)
      }
    }
    case (head :: tail) => {
      val (newState, newPreferences) = loop(List(head), state, preferences)
      loop(tail, newState, newPreferences)
    }
  }

  // Terminating conditions
  val (newState, newPreferences) = loop(matches, state, preferences)
  val isSingle = ""
  val emptyPairs = newState.filterKeys((k: String) => matches.contains(k)).filter(_._2 == isSingle).size
  if (emptyPairs == 0) {
    (newState, newPreferences)
  } else {
    val (newerState, newerPreferences) = makeMatches(matches, newState, newPreferences)
    (newState ++ newerState, newPreferences ++ newerPreferences)
  }
}

def stableMarriageProblem(males: List[String], preferences: Preferences): Any = {
  val (stableMatches, _) = makeMatches(males, Map[String, String](), preferences)
  val married = stableMatches.filterKeys((k: String) => males.contains(k))
  married.toList.map(i => Married(i._1, i._2))
}


val scoreTable = makeScoreTable(females)
val maleNames = males.toList.map(i => i._1)
println(stableMarriageProblem(maleNames, males))