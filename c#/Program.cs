using System;
using System.Collections.Generic;
using System.Linq;

namespace c_
{
    class Program
    {
        static void Main(string[] args)
        {
            Dictionary<string, List<string>> males = new Dictionary<string, List<string>>();
            males.Add("0", new List<string>{"7", "5", "6", "4"});
            males.Add("1", new List<string>{"5", "4", "6", "7"});
            males.Add("2", new List<string>{"4", "5", "6", "7"});
            males.Add("3", new List<string>{"4", "5", "6", "7"});

            Dictionary<string, List<string>> females = new Dictionary<string, List<string>>();
            females.Add("4", new List<string>{"0", "1", "2", "3"});
            females.Add("5", new List<string>{"0", "1", "2", "3"});
            females.Add("6", new List<string>{"0", "1", "2", "3"});
            females.Add("7", new List<string>{"0", "1", "2", "3"});

            Dictionary<string, string> stateTable = new Dictionary<string, string>();
            Dictionary<string, Dictionary<string, int>> scoreTable = MakeScoreTable(females);

            int totalMatches = 0;
            while (totalMatches != males.Keys.Count) {
                List<string> clonedMales = new List<string>(males.Keys);
                foreach (var male in clonedMales) {
                    var hasMale = stateTable.ContainsKey(male);
                    if (!hasMale) {
                        stateTable[male] = "";
                    } else {
                        var isMaleSingle = stateTable[male] == "";
                        if (isMaleSingle) {
                            var female = males[male].FirstOrDefault();
                            var hasFemale = stateTable.ContainsKey(female);
                            if (!hasFemale) {
                                stateTable[female] = "";
                            } else {
                                var isFemaleSingle = stateTable[female] == "";
                                if (isFemaleSingle) {
                                    stateTable[male] = female;
                                    stateTable[female] = male;
                                    totalMatches += 1;
                                } else {
                                    var currMale = stateTable[female];
                                    var score1 = scoreTable[female][male];
                                    var score2 = scoreTable[female][currMale];
                                    if (score1 < score2) {
                                        stateTable[currMale] = "";
                                        stateTable[male] = female;
                                        stateTable[female] = male;
                                    }
                                }
                            }
                            males[male] = males[male].Skip(1).ToList();
                        }
                    }
                }
            }

            foreach (var (key, value) in stateTable) {
                Console.WriteLine("{0} -> {1}", key, value);
            }
        }

        static private Dictionary<string, Dictionary<string, int>> MakeScoreTable (Dictionary<string, List<string>> females) {
            Dictionary<string, Dictionary<string, int>> scoreTable = new Dictionary<string, Dictionary<string, int>>();

            foreach (var (female, choices) in females) {
                int score = 0;
                Dictionary<string, int> maleScores = new Dictionary<string, int>();
                foreach (var male in choices) {
                    maleScores.Add(male, score);
                    score += 1;
                }
                scoreTable.Add(female, maleScores);
            }

            return scoreTable;
        }
    }
}