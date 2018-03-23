module ContextFree where

------------------------------------------------------------------------------------

data Cat = S | NP | VP | V | D | N | PP | P | IV | TV | RC deriving (Show,Eq)

data StrucDesc  = Binary Cat StrucDesc StrucDesc
                | Unary Cat StrucDesc
                | Leaf Cat String
                deriving Show

data GrammarRule    = BinaryStep Cat Cat Cat
                    | UnaryStep Cat Cat
                    | End Cat String
                    deriving Show

type Address = [Int]

grammar1 =  [   -- S rules
                BinaryStep S NP VP,
                -- VP rules
                BinaryStep VP V NP,
                BinaryStep VP V S,
                BinaryStep VP VP PP,
                UnaryStep  VP V,
                -- PP rules
                BinaryStep PP P NP,
                -- D rules
                End D "the",
                End D "a",
                -- N rules
                End N "cat",
                End N "dog",
                -- NP rules
                BinaryStep NP D N,
                End NP "John",
                End NP "Mary",
                -- V rules
                End V "left",
                End V "thinks",
                -- P rules
                End P "with"
            ]

grammar2 = [    BinaryStep S NP IV,
                BinaryStep NP N RC,
                UnaryStep NP N,
                BinaryStep RC NP TV,
                End N "dogs",   End N "cats",
                End IV "chase", End IV "sleep",
                End TV "chase"
            ]

sd1 = Binary S (Leaf NP "John") (Binary VP (Leaf V "left") (Leaf NP "Mary"))

sd2 = Binary S (Leaf NP "John")
               (Binary VP (Unary VP (Leaf V "left")) 
                          (Binary PP (Leaf P "with") (Binary NP (Leaf D "a") (Leaf N "cat")))
               )

sd3 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd1)

sd4 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd2)

sd5 = Binary S (Binary NP (Leaf N "dogs") (Binary RC (Unary NP (Leaf N "dogs")) (Leaf TV "chase"))) (Leaf IV "chase")

------------------------------------------------------------------------------------

pf :: StrucDesc -> [String]
pf (Binary c sd1 sd2) = pf sd1 ++ pf sd2
pf (Unary c sd) = pf sd
pf (Leaf c s) = [s]

leftmostLeaf :: StrucDesc -> String
leftmostLeaf (Leaf c s) = s
leftmostLeaf (Unary c sd) = leftmostLeaf sd
leftmostLeaf (Binary c sd1 sd2) = leftmostLeaf sd1

categoryOf :: StrucDesc -> Cat
categoryOf (Leaf c s) = c
categoryOf (Unary c sd) = c
categoryOf (Binary c sd1 sd2) = c

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Leaf c s) = elem c (enders g s)
wellFormed g (Unary c sd) = elem c (predecessorsUnary g (categoryOf sd)) && wellFormed g sd
wellFormed g (Binary c sd1 sd2) = elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) 
                                            && wellFormed g sd1 && wellFormed g sd2

depth :: StrucDesc -> Int
depth (Leaf c s) = 1
depth (Unary c sd) = 1 + depth sd
depth (Binary c sd1 sd2) = if (depth sd1 > depth sd2) then (1 + depth sd1) else (1 + depth sd2)

enders :: [GrammarRule] -> String -> [Cat]
enders [] x = []
enders (r:rs) x =
    case r of
    End c s -> if s == x then c : (enders rs x) else enders rs x
    UnaryStep c ch -> enders rs x
    BinaryStep c ch1 ch2 -> enders rs x

predecessorsUnary :: [GrammarRule] -> Cat -> [Cat]
predecessorsUnary [] x = []
predecessorsUnary (r:rs) x =
    case r of
    End c s -> predecessorsUnary rs x
    UnaryStep c ch -> if ch == x then (c : (predecessorsUnary rs x)) else (predecessorsUnary rs x)
    BinaryStep c ch1 ch2 -> predecessorsUnary rs x

predecessorsBinary :: [GrammarRule] -> (Cat,Cat) -> [Cat]
predecessorsBinary [] x = []
predecessorsBinary (r:rs) x =
    case r of
    End c s -> predecessorsBinary rs x
    UnaryStep c ch -> predecessorsBinary rs x
    BinaryStep c ch1 ch2 -> if (ch1,ch2) == x then (c : (predecessorsBinary rs x)) else (predecessorsBinary rs x)

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

brackets :: StrucDesc -> String
brackets (Binary c sd1 sd2) = "[" ++ brackets sd1  ++ " "++ brackets sd2 ++ "]"
brackets (Unary c sd) = "[" ++ brackets sd  ++ "]"
brackets (Leaf c s) =  s

labeledBrackets :: StrucDesc -> String
labeledBrackets (Binary c sd1 sd2) = "[" ++ show c ++ " " ++ labeledBrackets sd1 ++ " " ++ labeledBrackets sd2  ++ "]"
labeledBrackets (Unary c sd) = "[" ++ show c ++ " " ++ labeledBrackets sd  ++ "]"
labeledBrackets (Leaf c s) =  s

numNPs :: StrucDesc -> Int
numNPs (Leaf c s) =  if (show c == show NP) then 1 else 0
numNPs (Binary c sd1 sd2) = if (show c == show NP) then 1 + numNPs sd1 + numNPs sd2 else numNPs sd1 + numNPs sd2
numNPs (Unary c sd) =  if (show c == show NP) then 1 + numNPs sd else numNPs sd

numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations g (Leaf c s) = if (elem c (enders g s)) then 0 else 1
numViolations g (Unary c sd) =  if (elem c (predecessorsUnary g (categoryOf sd))) then numViolations g sd else 1  + numViolations g sd 
numViolations g (Binary c sd1 sd2) = if (elem c (predecessorsBinary g (categoryOf sd1,  categoryOf sd2))) then numViolations g sd1 + numViolations g sd2 else 1 + numViolations g sd1 + numViolations g sd2



sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap g (Leaf c s) = Leaf c (g s) 
sdMap g (Unary c sd) =  Unary c (sdMap g sd)
sdMap g (Binary c sd1 sd2) = Binary c (sdMap g sd1) (sdMap g sd2)


longestPath :: StrucDesc -> [Cat]
longestPath (Leaf c s) = [c] -- leaf then return [c]
longestPath (Unary c sd) =  c:longestPath sd -- Unary append using c:(recusive call unary )
longestPath (Binary c sd1 sd2) = 
    if (length (longestPath sd1) >= length (longestPath sd2))  --  compare the length using the length fun and recursively call longestpath
        then c : longestPath sd1
        else c : longestPath sd2


allPaths :: StrucDesc -> [[Cat]]
allPaths (Leaf c s) =  [[c]]
allPaths (Unary c sd) =  map(\p -> c:p) (allPaths sd)
allPaths (Binary c sd1 sd2) =  map(\p -> c:p) (allPaths sd1) ++ map(\p -> c:p) (allPaths sd2)


----addressesOfNPs function
addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs (Leaf c s) = if c == NP then [[]] else []
addressesOfNPs (Unary c sd) = 
    let rest = map (\a -> [0] ++ a) (addressesOfNPs sd) in
    if c == NP then [[]] ++ rest else rest
addressesOfNPs (Binary c sd1 sd2) = 
    let rest = map (\a -> [0] ++ a) (addressesOfNPs sd1) ++ map (\a -> [1] ++ a) (addressesOfNPs sd2) in
    if c == NP then [[]] ++ rest else rest
------end
-- helper function for ccommand 
check:: Address -> Address -> Address -> Address-> Bool
check (a:aa) (b:bb) a3 a4 = if (a==b && strlen a3 == 1) then False else (checkMorethan2 a3 a4)

checkMorethan2:: Address -> Address -> Bool
checkMorethan2 (a:aa) (b:bb) = if (a==b) then checkMorethan2 aa bb else True
checkMorethan2 [] (b:bb)  = False

strlen :: Address -> Int
strlen a1 = length a1

ccommand :: Address -> Address -> Bool
ccommand a1 a2 =  if (strlen a1 >= strlen a2) then False else check a1 a2 a1 a2
---------- end

-----replace function 
replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace sd [] r = r
--replace (Leaf c s) (n:ns) r = undefine
--replace (Unary c sd) (n:ns) r = 
--    if n == 0 then Unary c (replace sd ns r) else printT sd
--replace (Binary c sd1 sd2) (n:ns) r = 
--    case n of 
--        0 -> Binary c (replace sd1 ns r) sd2
--        1 -> Binary c sd1 (replace sd2 ns r)
--        _ -> printT1 sd1 sd2
replace sd (n:ns) r =  case sd of 
     Leaf c s -> printT sd 
     Unary c sd -> if (n == 0) then Unary c (replace sd ns r) else printT sd
     Binary c sd1 sd2 ->  
      case n of 
        0 -> Binary c (replace sd1 ns r) sd2
        1 -> Binary c sd1 (replace sd2 ns r)
        _ -> printT1 sd1 sd2

printT1 :: StrucDesc -> StrucDesc -> StrucDesc
printT1 sd1 sd2 = Binary (categoryOf sd1) (printT sd1) (printT sd2)

printT :: StrucDesc -> StrucDesc
printT sd = case sd of
    Leaf c s -> Leaf c s 
    Unary c sd1 -> Unary c (printT sd1)
    Binary c sd2 sd3 -> Binary c (printT sd2) (printT sd3)
---end

---helper functions for move 
move2 :: Address -> StrucDesc -> StrucDesc
move2 [] sd =  sd 
move2 (n:ns) (Leaf c s) = undefined
move2 (n:ns) (Unary c sd) =  if (n == 0) then Unary c (move2 ns sd) else undefined
move2 (n:ns) (Binary c sd1 sd2) = 
    case n of  
        0 -> (move2 ns sd1) 
        1 -> (move2 ns sd2) 

move1 :: Address -> StrucDesc -> StrucDesc
move1 [] sd = case sd of  
    Leaf c s -> Leaf c "t"
    Unary c sd -> Leaf c "t"
    Binary c sd1 sd2 ->  Leaf c "t"
move1 (n:ns) (Leaf c s) = undefined
move1 (n:ns) (Unary c sd) =  if (n == 0) then Unary c (move1 ns sd) else undefined
move1 (n:ns) (Binary c sd1 sd2) = 
    case n of  
        0 -> Binary c (move1 ns sd1) sd2 
        1 -> Binary c sd1 (move1 ns sd2)

move :: Address -> StrucDesc -> StrucDesc
move a sd  = Binary (categoryOf sd)(move2 a sd) (move1 a sd)  
----end 

--numUnaries::StrucDesc -> Int
--numUnaries (Leaf c s) = 0
--numUnaries (Unary c sd) = 1 + numUnaries sd 
--numUnaries (Binary c sd1 sd2) = numUnaries sd1 + numUnaries sd2

--collectNPs::StrucDesc -> [StrucDesc]
--collectNPs sd = 
--    (if categoryOf sd == NP then [sd] else [])
--    ++
--     case sd of 
--        Leaf c s -> []
--        Unary c sd -> collectNPs sd
--        Binary c sd1 sd2  -> collectNPs sd1 ++ collectNPs sd2

--getSubtree::StrucDesc -> Address -> StrucDesc
--getSubtree sd [] = sd
--getSubtree (Leaf  c s ) (n:ns) = undefined
--getSubtree (Unary c sd) (n:ns) = 
--    if n = 0 then (getSubtree sd ns) else undefined
--getSubtree (Binary c sd1 sd2 2)(n:ns) = case n of (0 -> getSubtree sd ns; 1-> getSubtree sd2 ns; x -> )


