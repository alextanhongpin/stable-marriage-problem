
const StableMatching = require('./stable-matching')

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
  const stableMatching = StableMatching(males, females)
  const results = stableMatching.match()
  console.log('results =>', results)
}
main()
