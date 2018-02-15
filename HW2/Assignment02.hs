module Assignment02 where

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six:: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

-- sumUpTo
sumUpTo :: Numb -> Numb
sumUpTo n =  case n of 
			Z -> Z
			S n' -> (add n (sumUpTo n'))

-- equal
equal :: Numb -> (Numb -> Bool)
equal a b =  case a of 
			Z -> case b of 
			 	 Z -> True 
			 	 S b' -> False
			S a' -> case b of 
			 		Z -> False
			 		S b' -> (equal a' b') 


-- difference 
difference :: Numb -> (Numb -> Numb)
difference a b = case a of 
				Z -> case b of 
					Z -> Z
					S b' -> b 
				S a' -> case b of 
						Z -> a 
						S b' -> difference a' b'

--total
total :: NumbList -> Numb
total a = case a of 
			EmptyNL -> Z
			NonEmptyNL n nl -> add n (total nl)

--incrementAll
incrementAll :: Numb -> (NumbList -> NumbList)
incrementAll a b = case b of 
				EmptyNL -> EmptyNL
				NonEmptyNL b nl -> NonEmptyNL (add a b) (incrementAll a nl)

--addToEnd
addToEnd :: Numb -> (NumbList -> NumbList)
addToEnd a b = case a of 
				Z -> b
				S n' -> case b of 
							EmptyNL -> NonEmptyNL a EmptyNL
							NonEmptyNL b nl -> NonEmptyNL b (addToEnd a nl)


-- lastElement 
lastElement :: NumbList -> Numb
lastElement a = case a of 
				EmptyNL -> Z
				NonEmptyNL b nl -> case nl of 
									EmptyNL -> b
									_ -> lastElement nl

--contains 
contains :: (Numb -> Bool) -> (NumbList -> Bool)
contains a l =  case l of 
				EmptyNL -> False
				NonEmptyNL l nl -> if (a l) then True else contains a nl


--remove 
remove :: (Numb -> Bool) -> (NumbList -> NumbList)
remove a l =  case l of 
				EmptyNL -> EmptyNL
				NonEmptyNL l nl -> if (a l) then remove a nl else NonEmptyNL l (remove a nl)

-- append 
append :: NumbList -> (NumbList -> NumbList)
append a b = case a of 
				EmptyNL -> b
				NonEmptyNL a nl -> NonEmptyNL a (append nl b)


-- prefix
prefix :: Numb -> (NumbList -> NumbList)
prefix n l = case n of 
				Z -> EmptyNL
				S n' -> case l of 
					EmptyNL -> EmptyNL
					NonEmptyNL l nl -> NonEmptyNL l (prefix n' nl)


						

					

					








	        