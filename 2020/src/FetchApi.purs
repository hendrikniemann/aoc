module FetchApi (fetchInput) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)


foreign import fetchInputImpl :: Int -> EffectFnAff String


fetchInput :: Int -> Aff String
fetchInput day = fromEffectFnAff (fetchInputImpl day)
