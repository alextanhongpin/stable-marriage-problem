
function main () {
  const males = [
    ['0', ['7', '5', '6', '4']],
    ['1', ['5', '4', '6', '7']],
    ['2', ['4', '5', '6', '7']],
    ['3', ['4', '5', '6', '7']]
  ]

  const females = [
    ['4', ['0', '1', '2', '3']],
    ['5', ['0', '1', '2', '3']],
    ['6', ['0', '1', '2', '3']],
    ['7', ['0', '1', '2', '3']]
  ]

  const status = {}
  const names = males.map(([name, _]) => name)
  const maleTable = males.reduce((obj, [name, choices]) => {
    obj[name] = choices
    return obj
  }, {})
  const scoreTable = makeScoreTable(females)

  const results = stableMarriageProblem(status, names, maleTable, scoreTable)
  console.log(results)
}

function stableMarriageProblem (status, names, maleTable, scoreTable) {
  const newStatus = names.reduce((status, male) => {
    const isMaleSingle = status[male] === undefined
    if (isMaleSingle) {
      // Check choices
      const [female, ...restFemales] = maleTable[male]
      maleTable[male] = restFemales
      const isFemaleSingle = status[female] === undefined
      if (isFemaleSingle) {
        status[female] = male
        status[male] = female
      } else {
        const currMale = status[female]
        const score1 = scoreTable.lookup({ male, female })
        const score2 = scoreTable.lookup({ male: currMale, female })
        if (score1 < score2) {
          // Make changes
          status[male] = female
          status[female] = male
          status[currMale] = ''
        }
      }
    }
    return status
  }, status)

  const isMatchedCount = names.filter((name) => {
    return newStatus[name]
  }).length

  if (isMatchedCount !== names.length) {
    return {...newStatus, ...stableMarriageProblem(newStatus, names, maleTable, scoreTable)}
  }
  return newStatus
}

class ScoreTable {
  constructor (females) {
    this.table = this.constructScoreTable(females)
  }
  constructScoreTable (females) {
    return females.reduce((obj, [female, choices]) => {
      if (!obj[female]) {
        obj[female] = {}
      }
      return choices.reduce((_obj, male, score) => {
        _obj[female][male] = score
        return _obj
      }, obj)
    }, {})
  }
  lookup ({ male, female }) {
    return (this.table[female] && this.table[female][male]) || Infinity
  }
}

function makeScoreTable (females) {
  return new ScoreTable(females)
}
main()
