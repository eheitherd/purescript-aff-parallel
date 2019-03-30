module Test.Main where

import Prelude

import Data.Array ((..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), catchError, delay, error, launchAff_, message, throwError)
import Effect.Aff.Parallel (parallelize, parallelize_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Random (randomRange)


main :: Effect Unit
main = launchAff_ do
  test1
  test2
  test3


test1 ∷ Aff Unit
test1 = do
  log "* parallelize_"
  parallelize_ 4 $ job <$> 1..10


test2 ∷ Aff Unit
test2 = do
  log "* parallelize"
  results ←  parallelize 4 $ job <$> 1..10
  logShow results


test3 ∷ Aff Unit
test3 = flip catchError (log <<< message) do
  log "* fail"
  parallelize_ 4 $ join
    [ job <$> 1..5
    , [ throwError $ error "Error" ]
    , job <$> 7..10
    ]


job ∷ Int → Aff Int
job n = do
  wait ← liftEffect $ randomRange 500.0 2000.0
  log $ "Begin: " <> show n
  delay $ Milliseconds wait
  log $ "Done: " <> show n
  pure n
