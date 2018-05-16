## Coding Standards

## Modules

### Imports

Imports should be done in this order :

1. Native modules & modules installed in *node_modules*
1. Local modules from a Yarn workspace
1. Local modules
1. Flow types from a yarn workspace
1. Flow types

**Bad:**
```javascript
import type { Car } from '../../../../../../cars';
import type { Id } from '@fluo/tech/flow';

import { diffInMonths } from '../../../../../../tech/time/diff';
import { updateDeductible } from '@fluo/back-office/src/main/contract/Contract';
import { buildDeductibleRows } from '@fluo/back-office/src/main/contract/deductibleRow';

import moment from 'moment';
```

**Good:**
```javascript
import moment from 'moment';

import { buildDeductibleRows } from '@fluo/back-office/src/main/contract/deductibleRow';
import { updateDeductible } from '@fluo/back-office/src/main/contract/Contract';

import { diffInMonths } from '../../../../../../tech/time/diff';

import type { Id } from '@fluo/tech/flow';

import type { Car } from '../../../../../../cars';
```

### Do not use wildcards imports

**Bad:**
```javascript
import * as contact from './contact';
```

**Good:**
```javascript
import { fetchContact } from './contact';
```

### Exports are placed at the function level

**Bad:**
```javascript
const computeEffectiveDate = rate => {}

export { computeEffectiveDate };
```

**Good:**
```javascript
export const computeEffectiveDate = rate => {}
```

## Objects

### Create default objects and use the spread operator to override values (beware sm)

**Bad:**
```javascript
const user = {
  firstName: 'John',
  lastName: 'Do',
};
user.lastName = 'Doe';
```

**Good:**
```javascript
const user = {
  firstName: 'John',
  lastName: 'Do',
};
const updatedUser = {
  ...user,
  lastName: 'Doe',
}
```

## Functions

### Don't use boolean arguments and add logic in a function (find a better example)

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
const bourquinEffectiveDate = () => {
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
const isNumberKeyPressed = keyCode => keyCode >= 48 && keyCode <= 57;

if (isNumberKeyPressed(keyCode)) {
  ...
}
```

## Error handling

### A function throwing should be visible in its name (find a better example: use a synchronous method)

**Bad:**
```javascript
// Since the function is exported, you have to look at the implementation to know it is throwing. It is easy to misread (especially when the function is too long) and not see the throw thus not wrapping it in a try catch when it is necessary

const fetchUserEmail = async(id) => {
  const { row: { email } } = await selectOne(
    sql`SELECT email FROM person WHERE id = ${id}`
  );

  if (!row) {
    throw new Error(`A person corresponding to the id ${id} was not found`);
  }

  ...
}
```

**Good:**
```javascript
// There is no excuse not wrapping the function call around a try catch when it is necessary

const fetchUserEmailOrThrow = async (id) => {
  const { row: { email } } = await selectOne(
    sql`SELECT email FROM person WHERE id = ${id}`
  );

  if (!row) {
    throw new Error(`A person corresponding to the id ${id} was not found`);
  }

  ...
}
```

## Tests

### Any domain object must be created with a factory (too early)

**Bad:**
```javascript
export const contract: ContractCover = {
  coverTypeId: COVER_TYPE_DAMAGE.id,
  deductible: null,
  limit: null,
  newLimit: null,
};
```

**Good:**
```javascript
type AContractCover = (props: $Shape<ContractCover>) => ContractCover
const aContractCover = props => ({
  coverTypeId: COVER_TYPE_DAMAGE.id,
  deductible: null,
  limit: null,
  newLimit: null,
  ...props,
});
```

## Constraints

+ No regex (unless you want to rewrite your code when you reach the fourth test case)

## Pair Programming guidelines

When holding the keyboard you:
+ should listen to your partner.
+ should think out loud when typing 
+ should let your partner guide you whenever possible (if you have too many ideas then do not hold the keyboard)

When not holding the keyboard you:
+ should guide the keyboard holder
+ should think out loud

Don’t start/answer the with a no; implement the idea and then discuss. It is ok to start from scratch if it is not a good one.

## Rules

1. Create a simple String calculator with a method (numbers: string => number) that can take 0, 1 or 2 numbers, and will return their sum (for an empty string it will return 0) for example “” or “1” or “1,2”

2. Allow the Add method to handle an unknown amount of numbers

3. Allow the Add method to handle new lines between numbers (instead of commas).
    + the following input is ok:  “1\n2,3”  (will equal 6)
    + the following input is NOT ok:  “1,\n” (not need to prove it - just clarifying)

4. Support different delimiters
    + to change a delimiter, the beginning of the string will contain a separate line that looks like this: “//[delimiter]\n[numbers…]” for example “//8\n182” should return three where the default delimiter is ‘;’
    + the first line is optional. all existing scenarios should still be supported

5. Calling Add with a negative number will throw an exception “negatives not allowed” - and the negative that was passed. If there are multiple negatives, show all of them in the exception message

6. BREAK

7. Numbers bigger than 1000 should be ignored, so adding 2 + 1001  = 2

8. Delimiters can be of any length with the following format:  “//[delimiter]\n” for example: “//[8]\n1***20***3” should return 24

9. Allow multiple delimiters like this:  “//[delim1][delim2]\n” for example “//[*][6]\n1*263” should return 6

10. Make sure you can also handle multiple delimiters with length longer than one char

## Ressources

+ [String Calculator](http://osherove.com/tdd-kata-1/)