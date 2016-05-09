
--use ++ operator to connect lists
--use : cons operator to connect element with a list
lostNumbers = [1,2,3]
fiveNumbers = lostNumbers ++ [4,5]
sixNumbers = 0 : fiveNumbers
nums = 0:1:2:3:[]
hello = "Hello"
world = "world"
helloWorld = hello ++ ", " ++ world
a = 'A'
smallCat = a : " small cat"

--use !! operator to get an element out of a list
one = nums !! 1
two = nums !! 2
three = nums !! 3

--head, tail, last, init operation and so on
headOfNums = head nums
tailOfNums = tail nums
lastOfNums = last nums
initOfNums = init nums

lengthOfNums = length nums

--null to check if a list is empty
--return False
checkIfNumsEmpty = null nums
--return True
checkEmptyList = null []

--reverse a list
reversedNums = reverse nums

--takes the first n elements
--[0,1]
firstTwoOfNums = take 2 nums
--[0,1,2]
firstThreeOfNums = take 3 nums
--[]
noneOfNums = take 0 nums
--all elements
allOfNums = take 99 nums

--drops n elements from begining
--drop 0
dropZeroOfNums = drop 0 nums
--[1,2,3]
dropOneOfNums = drop 1 nums
--[2,3]
dropTwoOfNums = drop 2 nums
--[3]
dropThreeOfNums = drop 3 nums
--[]
dropAllOfNums = drop 99 nums

--return the biggest
biggestOfNums = maximum nums
--return the smallest
smallestOfNums = minimum nums

--sum of a list of NUMBERS
sumOfNums = sum nums
--product of a list of NUMBERS
productOfNums = product nums
productOfLostNumbers = product lostNumbers

--check if a thing is an element of the list
containA = 'A' `elem` smallCat
containsA = elem 'A' smallCat
containZero = 0 `elem` lostNumbers
containsZero = elem 0 lostNumbers
containZeroInNums = 0 `elem` nums
containsZeroInNums = elem 0 nums

--list comprehension
evensInTwenty = [x*2 | x<-[1..10]]
evensBiggerThanTwelve = [x*2 | x<-[1..10], x*2 >= 12]
devidedBySevens = [x|x<-[50..100],x `mod` 7 == 3]
boomBangs xs = [if x<10 then "BOOM!" else "BANG!" | x<-xs,odd x]
predicatedList = [x|x<-[10..20],x/=13,x/=15,x/=19]
multipliedList = [x*y|x<-[1,2,3],y<-[7,8,9]]
multipliedBiggerThanFifty = [x*y | x<-[2,5,10], y<-[8,10,11], x*y>50]
nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
decoratedNouns = [adjective ++ " " ++ noun|adjective <- adjectives, noun <-nouns]
len xs = sum [1|_<-xs]
removeNonLetters st = [c|c<-st, c `elem` ['A'..'Z']++['a'..'z']]

