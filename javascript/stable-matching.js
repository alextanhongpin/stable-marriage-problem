function toObject (arr) {
  return arr.reduce((obj, [head, tail]) => {
    obj[head] = tail
    return obj
  }, {})
}

function takeFirst (arr) {
  return arr.map(([head, _]) => head)
}

function createHashTable (arr) {
  return arr.reduce((obj, [head, tail]) => {
    if (!obj[head]) {
      obj[head] = {}
    }
    return tail.reduce((_obj, item, score) => {
      _obj[head][item] = score
      return _obj
    }, obj)
  }, {})
}

class StableMatching {
  constructor (males, females) {
    this.maleNames = takeFirst(males)
    this.maleChoices = toObject(males)
    this.scores = createHashTable(females)
  }
  compareScore (male, female) {
    return (this.scores[male] && this.scores[male][female]) || Infinity
  }
  engage (state, male, female) {
    state[male] = female
    state[female] = male
  }
  breakup (state, male) {
    state[male] = ''
  }
  currentPartner (state, person) {
    return state[person]
  }
  isSingle (state, partner) {
    return state[partner] === undefined
  }
  getPreference (male) {
    return (this.maleChoices[male] && this.maleChoices[male][0]) || null
  }
  updatePreferences (male) {
    const preferences = [...this.maleChoices[male]]
    this.maleChoices[male] = preferences.slice(1, preferences.length)
  }
  terminationCondition (state) {
    const currentMatches = this.maleNames.filter((name) => {
      return state[name]
    }).length
    return currentMatches === this.maleNames.length
  }
  match (initialState = {}) {
    const names = this.maleNames
    const loop = (initialState = {}) => {
      return names.reduce((state, male) => {
        if (!this.isSingle(state, male)) {
          return state
        }
        const female = this.getPreference(male)
        if (this.isSingle(state, female)) {
          this.engage(state, male, female)
        } else {
          const currMale = this.currentPartner(state, female)
          const score1 = this.compareScore(male, female)
          const score2 = this.compareScore(currMale, female)
          if (score1 < score2) {
            this.breakup(state, currMale)
            this.engage(state, male, female)
          }
        }
        this.updatePreferences(male)
        return state
      }, initialState)
    }
    const state = loop(initialState)
    if (!this.terminationCondition(state)) {
      return {...state, ...this.match(state)}
    }
    return state
  }
}

module.exports = (male, females) => {
  return new StableMatching(male, females)
}
