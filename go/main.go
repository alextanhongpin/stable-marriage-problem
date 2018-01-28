package main

import "log"

type StatusTable struct {
	Status    map[string]string
	Scores    map[string]map[string]int
	MaleNames []string
}

func (s *StatusTable) IsStable() bool {
	return len(s.CheckStatus()) == len(s.MaleNames)
}
func (s *StatusTable) CheckStatus() map[string]string {
	status := make(map[string]string)
	for _, male := range s.MaleNames {
		if s.Status[male] != "" {
			status[male] = s.Status[male]
		}
	}
	return status
}
func (s *StatusTable) CheckMaleStatus(male string) string {
	return s.Status[male]
}
func (s *StatusTable) CheckFemaleStatus(female string) string {
	return s.Status[female]
}
func (s *StatusTable) Engage(male, female string) {
	s.Status[male] = female
	s.Status[female] = male
}

func (s *StatusTable) Breakup(male string) {
	s.Status[male] = ""
}

func (s *StatusTable) ShouldReengage(female, male1, male2 string) bool {
	score1 := s.Scores[female][male1]
	score2 := s.Scores[female][male2]
	reengage := score1 < score2
	return reengage
}

func makeStatusTable(females map[string][]string, maleNames []string) *StatusTable {
	return &StatusTable{
		Status:    make(map[string]string),
		Scores:    makeScoreTable(females),
		MaleNames: maleNames,
	}
}

func main() {
	maleNames := []string{"0", "1", "2", "3"}

	males := make(map[string][]string)
	males["0"] = []string{"7", "5", "6", "4"}
	males["1"] = []string{"5", "4", "6", "7"}
	males["2"] = []string{"4", "5", "6", "7"}
	males["3"] = []string{"4", "5", "6", "7"}

	females := make(map[string][]string)
	females["0"] = []string{"0", "1", "2", "3"}
	females["1"] = []string{"0", "1", "2", "3"}
	females["2"] = []string{"0", "1", "2", "3"}
	females["3"] = []string{"0", "1", "2", "3"}

	statusTable := makeStatusTable(females, maleNames)

	for !statusTable.IsStable() {
		for _, male := range maleNames {
			if statusTable.CheckMaleStatus(male) == "" {
				female := males[male][0]
				remainingFemales := males[male][1:len(males[male])]
				engagedMale := statusTable.CheckFemaleStatus(female)
				if engagedMale == "" {
					statusTable.Engage(male, female)
				} else {
					if statusTable.ShouldReengage(female, male, engagedMale) {
						statusTable.Engage(male, female)
						statusTable.Breakup(engagedMale)
					}
				}
				males[male] = remainingFemales
			}
		}
	}

	log.Println(statusTable.CheckStatus())
	log.Println(statusTable.IsStable())
}

// makeScoreTable returns a table that allows user to search for the score of the male by female
func makeScoreTable(females map[string][]string) map[string]map[string]int {
	scoreTable := make(map[string]map[string]int)
	for female := range females {
		for i, male := range females[female] {
			if scoreTable[female] == nil {
				scoreTable[female] = make(map[string]int)
			}
			scoreTable[female][male] = i
		}
	}
	return scoreTable
}
