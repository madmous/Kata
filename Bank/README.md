## Rules

+ Deposits and withdrawals can be maded into an account
+ Each deposit or withdrawal will have the amount of the operation and the date of the operation
+ You should be able to transfer between accounts. A transfer will appear as a withdrawal in the account of the transferor and as a deposit on the account of the transferee.
+ A statement can be requested at any time. The statement will contain for each entry the date, the amount of deposition, the amount of withdrawal (only one of the two should have a value), and the balance of the account after the entry.
+ Headers should be shown on the statement.
+ You should be able to filter the statement (only deposits, only withdrawals, date)

## Changes

+ The information must be stored on a file or database.

## Coding standards

### No mutable state

### Functions should be 50 lines long

### Test functions should be 30 lines long

### One assertion per test

### Always use function expression and type them

**Bad:**
```javascript
function addRoverToGame(game: Game, rover: Rover): Game {
  return addToGame('r', game, rover.coordinate);
}
```

**Good:**
```javascript
type AddRovertoGame = (game: Game, rover: Rover) => Game;

const addRoverToGame: AddRovertoGame = (game, rover) => {
  return addToGame('r', game, rover.coordinate);
}
```

### Name everything (meaningful, searchable, explanatory)

**Bad:**
```javascript
// What the heck is 86400000 for?
setTimeout(blastOff, 86400000);
```

**Good:**
```javascript
// Declare them as capitalized named constants.
const MILLISECONDS_IN_A_DAY = 86400000;

setTimeout(blastOff, MILLISECONDS_IN_A_DAY);
```

### Functions should not take any boolean argument

**Bad:**
```javascript
const effectiveDate = isBourquin => {
  if (isBourquin) {
    ...
  }
}
```

**Good:**
```javascript
const bourquinEffectiveDate = () {
  ...
}
```

### Encapsulate conditionals

**Bad:**
```javascript
if (keyCode >= 48 && keyCode <= 57) {
  ...
}
```

**Good:**
```javascript
const isNumberKeyPressed = keyCode => {
  return keyCode >= 48 && keyCode <= 57;
}

if (isNumberKeyPressed(keyCode)) {
  ...
}
```

### Avoid negative conditionals

**Bad:**
```javascript
const isDOMNodeNotPresent = node => {
  // ...
}

if (!isDOMNodeNotPresent(node)) {
  // ...
}
```

**Good:**
```javascript
const isDOMNodePresent = node => {
  // ...
}

if (isDOMNodePresent(node)) {
  // ...
}
```

### Use default arguments instead of short circuiting or conditionals

**Bad:**
```javascript
const createMicrobrewery = name => {
  const breweryName = name || 'Hipster Brew Co.';
  // ...
}
```

**Good:**
```javascript
const createMicrobrewery = (name = 'Hipster Brew Co.') => {
  // ...
}
```

### Always put default parameters last

**Bad:**
```javascript
const handleThings = (opts = {}, name) => {
  // ...
}
```

**Good:**
```javascript
const handleThings = (name, opts = {})  =>{
  // ...
}
```

### Never mutate parameters

**Bad:**
```javascript
const f1 = obj => {
  obj.key = 1;
}
```

**Good:**
```javascript
const f2 = obj => {
  const newObj = {
    ...obj,
    key: 1
  }
}
```