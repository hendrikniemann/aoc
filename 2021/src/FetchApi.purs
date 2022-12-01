module FetchApi (fetchInput) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)


foreign import fetchInputImpl :: String -> EffectFnAff String


fetchInput :: String -> Aff String
fetchInput day = fromEffectFnAff (fetchInputImpl day)
