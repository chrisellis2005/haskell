import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

divisbleBy x y = x `rem` y == 0
isFizz x = divisbleBy x 3
isBuzz x = divisbleBy x 5 
answer x
	| isFizz x && isBuzz x = "fizzbuzz"
	| isFizz x = "fizz"
	| isBuzz x = "buzz"
	| otherwise = show x
answers x = map answer x 

main :: IO ()
main =  hspec $ do
	describe "Given fizzbuzz" $ do
		context "When calling answer" $ do
			it "should returns fizz for number 3" $ do
				answer 3 `shouldBe` "fizz"

			it "should returns buzz for number 5" $ do
				answer 5 `shouldBe` "buzz"

			it "should returns fizz buzz for number 15" $ do
				answer 15 `shouldBe` "fizzbuzz"

			it "should return the number for number 1" $ do
				answer 1 `shouldBe` "1"

		context "when calling answers [1,2,3,4,5,15]" $ do
			it "should return [1,2,fizz,4,buzz,fizzbuzz]" $ do
				answers [1,2,3,4,5,15] `shouldBe` ["1","2","fizz","4","buzz","fizzbuzz"]

		context "when calling divisbleBy" $ do
			it "should return True when numbers are divisble" $ do
				divisbleBy 3 3 `shouldBe` True
			it "should return False when numbers are divisble" $ do
				divisbleBy 2 3 `shouldBe` False

		context "when calling isFizz" $ do
			it "should return True for a number divisbleBy 3" $ do
				isFizz 3 `shouldBe` True
			it "should return False for a number not divisbleBy 3" $ do
				isFizz 2 `shouldBe` False
				
		context "when calling isBuzz" $ do
			it "should return True for a number divisbleBy 5" $ do
				isBuzz 5 `shouldBe` True
			it "should return False for a number not divisbleBy 5" $ do
				isBuzz 4 `shouldBe` False