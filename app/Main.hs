{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main where
import           Data.Vector.Unboxed            ( singleton )
import           Lib                            ( mapTuple3 )
import           Statistics.Distribution
import           Statistics.Distribution.Beta
import           Statistics.Distribution.Exponential
import           System.Random.MWC              ( GenIO
                                                , Seed
                                                , asGenST
                                                , createSystemRandom
                                                , restore
                                                , save
                                                , toSeed
                                                , uniformVector
                                                , withSystemRandom
                                                )

generator :: IO GenIO
generator = createSystemRandom

-- get a 1000 sample of arrivalTime (equal for every customer type) and processingTime (one for each customerType, yellow, red, black)
getSample :: Seed -> Int -> IO [(Int, Int, Int, Int)]
getSample seed iteration = do
  generator          <- restore seed
  arrivesAt          <- genContVar (exponential 0.01) generator -- probability of customer attiving a time t behaves like an exponential with param 1/𝛼 (aka 0.01)
  yellowCustomerTime <- genContVar (betaDistr 2.0 5.0) generator
  redCustomerTime    <- genContVar (betaDistr 2.0 2.0) generator
  blackCustomerTime  <- genContVar (betaDistr 5.0 1.0) generator
  newSeed            <- save generator
  if iteration >= 1000
    then pure
      [ ( round arrivesAt
        , round $ 200 * yellowCustomerTime
        , round $ 200 * redCustomerTime
        , round $ 200 * blackCustomerTime
        )
      ]
    else (getSample newSeed $ iteration + 1) >>= \nextRes -> pure $ concat
      [ nextRes
      , [ ( round arrivesAt
          , round $ 200 * yellowCustomerTime
          , round $ 200 * redCustomerTime
          , round $ 200 * blackCustomerTime
          )
        ]
      ]

-- returns a tuple with the arrivalTime (equal for every customer type), departureTime and waitTime (diferent for each customer, yellow, red, black)
computeTimes
  :: [(Int, (Int, Int, Int), (Int, Int, Int))]
  -> (Int, Int, Int, Int)
  -> [(Int, (Int, Int, Int), (Int, Int, Int))]
computeTimes [] (a, y, r, b) = [(a, (y, r, b), (0, 0, 0))]
computeTimes ((aa, (yd, rd, bd), (yw, rw, bw)) : xs) (a, y, r, b) = concat
  [[(arrival, departure, wait)], [(aa, (yd, rd, bd), (yw, rw, bw))], xs]

 where
  arrival   = aa + a
  wait      = (yw + (max (y - a) 0), rw + (max (r - a) 0), bw + (max (b - a) 0))
  departure = mapTuple3 (\n -> n + arrival) wait

-- transform relative times to absolute ones
getAbsoluteTimes
  :: [(Int, Int, Int, Int)] -> [(Int, (Int, Int, Int), (Int, Int, Int))]
getAbsoluteTimes [] = []
getAbsoluteTimes t  = foldl computeTimes [] t

-- calculate the queue length at each step and then resurns the weighted average, max, min for each customer type
-- getAverageQueueLenght :: [(Int, (Int, Int, Int), (Int, Int, Int))] -> ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))

-- calculate the average/max/min waiting time for each customer type
-- getAverageWait :: [(Int, (Int, Int, Int), (Int, Int, Int))] -> ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))

main :: IO ()
main = do
  sample <- getSample (toSeed $ singleton 42) 1
  print $ getAbsoluteTimes sample
