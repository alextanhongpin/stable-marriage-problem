# Stable Marriage/Matching Problem

Reference to wiki link [here](https://en.wikipedia.org/wiki/Stable_marriage_problem).

## Algorithm

Pseudocode:

```
function stableMatching {
    Initialize all m ∈ M and w ∈ W to free
    while ∃ free man m who still has a woman w to propose to {
       w = first woman on m’s list to whom m has not yet proposed
       if w is free
         (m, w) become engaged
       else some pair (m', w) already exists
         if w prefers m to m'
            m' becomes free
           (m, w) become engaged 
         else
           (m', w) remain engaged
    }
}
```


Implementation in JavaScript:

```js
const menPreferences = {
  A: 'YXZ',
  B: 'ZYX',
  C: 'XZY'
}

const womenPreferences = {
  X: 'BAC',
  Y: 'CBA',
  Z: 'ACB'
}

const engaged = {}

const matches = Object.entries(menPreferences)
  .map(([man, preferences]) => [man, preferences.split('')])

while (matches.length) {
  const [man, preferences] = matches.shift()
  const woman = preferences.shift()
  if (!engaged[woman]) {
    engaged[woman] = man
    engaged[man] = woman
  } else {
    const currentMan = engaged[woman]
    const womanPreferences = womenPreferences[woman]
    if (womanPreferences.indexOf(man) < womanPreferences.indexOf(currentMan)) {
      engaged[woman] = man
      engaged[currentMan] = null
    }
  }
}

console.log(engaged)
```
