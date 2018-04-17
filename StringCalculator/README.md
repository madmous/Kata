## Constraints

+ Write the best code you have ever written

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
    + to change a delimiter, the beginning of the string will contain a separate line that looks like this: “//[delimiter]\n[numbers…]” for example “//;\n1;2” should return three where the default delimiter is ‘;’
    + the first line is optional. all existing scenarios should still be supported

5. Calling Add with a negative number will throw an exception “negatives not allowed” - and the negative that was passed. If there are multiple negatives, show all of them in the exception message

6. BREAK

7. Numbers bigger than 1000 should be ignored, so adding 2 + 1001  = 2

8. Delimiters can be of any length with the following format:  “//[delimiter]\n” for example: “//[***]\n1***2***3” should return 6

9. Allow multiple delimiters like this:  “//[delim1][delim2]\n” for example “//[*][%]\n1*2%3” should return 6

10. Make sure you can also handle multiple delimiters with length longer than one char

## Ressources

+ [String Calculator](http://osherove.com/tdd-kata-1/)