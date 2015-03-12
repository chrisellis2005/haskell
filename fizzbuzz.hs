module fizzbuzz(fizzbuzz) where
divisbleBy x y = x `rem` y == 0
isFizz x = divisbleBy x 3
isBuzz x = divisbleBy x 5 
answer x
	| isFizz x && isBuzz x = "fizzbuzz"
	| isFizz x = "fizz"
	| isBuzz x = "buzz"
	| otherwise = show x
answers x = map answer x 