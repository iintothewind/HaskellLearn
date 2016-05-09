
nums = [0..9]
lowerCaseLetters = ['a'..'z']
upperCaseLetters = ['A'..'Z']

--[1,3,5,7,9], start from 1, add 2 by each next, limit to 9
odds = [1,3..9]
--[2,4,6,8], start from 2, add 2 by each next, limit to 9
evens = [2,4..9]

infiniteNums = [0,1..]
tenOfInfiniteNums = take 10 infiniteNums

--cycle, takes a list and cycle it into an infinite list
tenOfCycle = take 10 (cycle [1,2,3])
threeLols = take 12 (cycle "LOL ")

--repeat, takes an element and produce an infinite list of that element
tenOfRepeatFive = take 10 (repeat 5)

--replicate, create some number of same element in a list
threeSameLetters = replicate 3 'A'

