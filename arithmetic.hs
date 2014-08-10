{- Type of a dyadic rational, a bit like floating point except that operations can be performed exactly.

  resource tells you which maximum denominator to use for inexact operations
  (exact operations send dyadic rationals to dyadic rationals.)

 -}

data DyadicRational = DyadicRational {num :: Int, denom :: Int, maxDenom :: Int} deriving Show

instance Eq DyadicRational where
        x == y = (num x * denom y) == (num y * denom x)

instance Ord DyadicRational where
	compare x y = (num x * denom y) `compare` (num y * denom x)

resource = 10 :: Int

fromInt x = DyadicRational {num = x, denom = 0, maxDenom = resource}

-- does not perform any simplification

add :: DyadicRational -> DyadicRational -> DyadicRational
add x y = 
	let expz = max (denom x) (denom y) in
	let numz = num x * 2 ^ (expz - denom x) + num y * 2 ^ (expz - denom y) in
	let maxDenomz = max (maxDenom x) (maxDenom y) in
	DyadicRational {num = numz , denom = expz , maxDenom = maxDenomz}

-- does not perform any simplification, almost identical to add.

sub :: DyadicRational -> DyadicRational -> DyadicRational
sub x y = 
	let expz = max (denom x) (denom y) in
	let numz = num x * 2 ^ (expz - denom x) - num y * 2 ^ (expz - denom y) in
	let maxDenomz = max (maxDenom x) (maxDenom y) in
	DyadicRational {num = numz , denom = expz , maxDenom = maxDenomz}

-- also doesn't perform simplification

mul :: DyadicRational -> DyadicRational -> DyadicRational
mul x y =
	let expz = denom x + denom y in
	let maxDenomz = max (maxDenom x) (maxDenom y) in
	let numz = num x * num y in
	DyadicRational {num = numz , denom = expz , maxDenom = maxDenomz}

-- lower approximation to integer reciprocal
recpFloor :: Int -> DyadicRational
recpFloor k = 
	let numz = (2^resource) `div` k in -- constructs a lower approximation.
	DyadicRational {num = numz, denom = resource, maxDenom = resource}

-- upper approximation to integer reciprocal
recpCeil :: Int -> DyadicRational
recpCeil k =
	let numz = (2^resource) `div` k + (if (2^resource) `mod` k == 0 then 0 else 1) in
	DyadicRational {num = numz, denom = resource, maxDenom = resource}

recpFloor1 :: DyadicRational -> DyadicRational
recpFloor1 k =
	(recpFloor $ num k) `mul` (fromInt (2^ denom k) )

recpCeil1 :: DyadicRational -> DyadicRational
recpCeil1 k =
	(recpCeil $ num k) `mul` (fromInt (2^ denom k) )

toDouble :: DyadicRational -> Double
toDouble x =
	let num' = (fromIntegral $ num x) :: Double in
	let denom' = (fromIntegral $ denom x) :: Double in
	num' / 2 ** denom'


data Interval = Interval {lower :: DyadicRational, upper :: DyadicRational}

instance Show Interval where
	show x = if lower x == upper x
		then "{" ++ (show $ toDouble $ lower x) ++ "}"
		else "{" ++ (show $ toDouble $ lower x) ++ ", " ++ (show $ toDouble $ upper x) ++ "}"


instance Eq Interval where
	x == y = (lower x == lower y) && (upper x == upper y)

-- implementation of add is unnecessarily slow but IDGAF

-- note, I HAD to define signum. currently it just checks whether the interval contains zero
-- which is actually quite handy. Note that abs will not do the right thing.
-- I'll make a better version later.

instance Num Interval where
	x + y = apply2 add x y
	x - y = apply2 sub x y
	x * y = apply2 mul x y
	signum x = if (lower x > fromInt 0) then 1 else if (upper x < fromInt 0) then -1 else 0

	abs x = case signum x of 
		1 -> x
		(-1) -> (fromInteger 0) - x
		0 -> Interval {lower = fromInt 0, upper = max (fromInt (-1) `mul` lower x) (upper x) }

	fromInteger x = promoteDyadicRational $ fromInt $ fromInteger x

instance Fractional Interval where
	recip x = Interval {lower = recpFloor1 $ upper x , upper = recpCeil1 $ lower x}
	fromRational x = error "seriously you guys"
	
toList :: Interval -> [DyadicRational]
toList i0 = [lower i0, upper i0]

promoteDyadicRational :: DyadicRational -> Interval
promoteDyadicRational x = Interval {lower = x, upper = x}

apply2 :: (DyadicRational -> DyadicRational -> DyadicRational) -> Interval -> Interval -> Interval
apply2 f i0 i1 =
	let vals = [ f x y | x <- toList i0 , y <- toList i1 ] in
	Interval {lower = minimum vals, upper = maximum vals}
