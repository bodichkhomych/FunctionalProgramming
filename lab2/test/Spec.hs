import           Test.Tasty           (defaultMain, testGroup)
import           Test.Tasty.HUnit     (assertEqual, assertFailure,assertBool,testCase)
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies

calculate :: Double -> Double -> Double -> Int ->(Double->Double)-> Double
calculate a b eps p = calc a b (eps / fromIntegral p) p ((b - a) / fromIntegral p)


calc :: Double -> Double -> Double -> Int -> Double ->(Double->Double)-> Double
calc a b eps p h func=
  let chunks =
        [ integrate
          (a + (h * fromIntegral i))
          (a + (h * (fromIntegral i + 1)))
          h
          eps
          (trap (a + (h * fromIntegral i)) (a + (h * (fromIntegral i + 1))) h func)
          (trap (a + (h * fromIntegral i)) (a + (h * (fromIntegral i + 1))) (h / 2.0) func)
          func
        | i <- [0 .. p - 1]
        ] `using` parList rdeepseq
   in sum chunks

integrate :: Double -> Double -> Double -> Double -> Double -> Double ->(Double->Double)-> Double
integrate a b step eps previous current func
    | runge previous current eps = current
  | otherwise = integrate a b (step / 2.0) eps (trap a b (step / 2.0) func) (trap a b (step / 4.0) func) func

trap :: Double -> Double -> Double ->(Double->Double)-> Double
trap a b step func = step * ((func a + func b)/2.0 + summ (a + step) (b - step) step func)

summ :: Double -> Double -> Double ->(Double->Double) ->Double
summ a b step func| a > b = 0
                 | a == b = func a
                 | otherwise = (func a + func b) + summ (a + step) (b - step) step func

runge :: Double -> Double -> Double -> Bool
runge h1 h2 eps | abs(h1 - h2) < eps = True
                  | otherwise = False

func :: Double -> Double
func x = 4 * sin x

linear :: Double->Double
linear x=25*x+10

main = defaultMain allTests

allTests=testGroup "Integral tests"[sinTest,linearTest]

sinTest =
  testCase "Sinus function integration test" $
  assertBool "Value not in range in sinus test" (7.999 <= calculate 0 pi 0.0001 8 func && calculate 0 pi 0.001 8 func <=8.001)

linearTest =
  testCase "Linear function integration test" $
  assertBool "Value not in range in linear test" (959.999<= calculate (-6) 10 0.0001 8 linear && calculate (-6) 10 0.001 8 linear <=960.001)