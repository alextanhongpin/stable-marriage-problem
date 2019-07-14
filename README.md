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
// Solution 1: "{"Y":"A","A":"Y","Z":"B","B":"Z","X":"C","C":"X"}"
// const menPreferences = {
//   A: 'YXZ',
//   B: 'ZYX',
//   C: 'XZY'
// }

// const womenPreferences = {
//   X: 'BAC',
//   Y: 'CBA',
//   Z: 'ACB'
// }


// Solution 2: "{"0":"7","1":"5","2":"4","3":"6","4":"2","5":"1","6":"3","7":"0"}"
const engaged = {}

// const menPreferences = {
//   0: '7564',  
//   1: '5467',  
//   2: '4567', 
//   3: '4567'
// }

// const womenPreferences = {
//   4: '0123',  
//   5: '0123',  
//   6: '0123',  
//   7: '0123'
// }

// Solution 3: "{"X":"A","A":"X","Y":"B","B":"Y","Z":"C","C":"Z"}"

const menPreferences = {
  A: 'XYZ',
  B: 'YXZ',
  C: 'XYZ'
}

const womenPreferences = {
  X: 'BAC',
  Y: 'ABC',
  Z: 'ABC'
}

const bachelors = Object.entries(menPreferences)
  .map(([man, preferences]) => [man, preferences.split('')])

while (bachelors.length) {
  const [man, preferences] = bachelors.shift()
  const woman = preferences.shift()
  if (!engaged[woman]) {
    engaged[woman] = man
    engaged[man] = woman
  } else {
    const currentMan = engaged[woman]
    const womanPreferences = womenPreferences[woman]
    // Smaller index value is more preferable.
    if (womanPreferences.indexOf(man) < womanPreferences.indexOf(currentMan)) {
      engaged[woman] = man
      engaged[man] = woman
      delete engaged[currentMan]
    } else {
      // Reject.
      console.log('man is rejected', man)
      bachelors.unshift([man, preferences])
    }
  }
}

console.log(engaged)
```
