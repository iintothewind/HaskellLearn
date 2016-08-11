module Haskell.Tuples() where 
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y

--fst takes a pair and returns its first component
firstComponent1 = fst (8,100)
firstComponent2 = fst ("Wow",False)
--snd takes a pair and returns its second component
secondComponent1 = snd (8,100)
secondComponent2 = snd ("Wow",False)

--fst and snd only work on pairs
fstFirst = fst (fst ((1,2),3))
sndSecond = snd (snd (1,(2,3)))

--zip takes two lists and zip them together as a list of pairs
fivePairs = zip [1..5] ["one","two","three","four","five"]
zipFiniteListsWithInfiniteLists = zip [1..] ["apple","orange","cherry","mango"]

--triples
triagles = [(a,b,c)|a<-[1..10],b<-[1..10],c<-[1..10]]
rightTriagles = [(a,b,c)|a<-[1..10],b<-[1..10],c<-[1..10],a^2+b^2==c^2]
correctTriagles = [(a,b,c)|c<-[1..10],b<-[1..c],a<-[1..b],a^2+b^2==c^2,a+b+c==24]

