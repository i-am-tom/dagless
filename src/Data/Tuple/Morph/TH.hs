{-|
Module      : Data.Tuple.Morph.TH
Description : The dirty @TemplateHaskell@ internals of tuple-morphing.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

https://github.com/i-am-tom/learn-me-a-haskell/blob/master/src/Utils/Tuple/TH.hs

This module is ripped straight from my other repository, where you'll find a
lot more documentation. Again, this will be replaced by tuple-morph as soon
as possible.
-}
module Data.Tuple.Morph.TH where

import Data.Foldable       (foldl')
import Data.Function       ((&))
import Language.Haskell.TH

-- | The type of the tuple in the morph instance.
mkTupleT :: [Name] -> Type
mkTupleT xs = foldl' (\head -> AppT head . VarT) (TupleT (length xs)) xs

-- | The type of the HList in the morph instance.
mkHListT :: [Name] -> Type
mkHListT = foldr (AppT . AppT PromotedConsT . VarT) PromotedNilT

-- | The tuple pattern match.
mkTupleP :: [Name] -> Pat
mkTupleP = TupP . fmap VarP

-- | The HList pattern match.
mkHListP :: [Name] -> Pat
mkHListP (x : xs) = ConP (mkName  ":>" ) [VarP x, mkHListP xs]
mkHListP []       = ConP (mkName "HNil") []

-- | The tuple expression.
mkTupleE :: [Name] -> Exp
mkTupleE = TupE . fmap (Just . VarE)

-- | The HList expression.
mkHListE :: [Name] -> Exp
mkHListE = foldr (AppE . AppE cons . VarE) nil
  where
    nil  = ConE (mkName  "HNil")
    cons = ConE (mkName   ":>" )

-- | All together now!
makeMorphInstance :: Int -> Q Dec
makeMorphInstance count = do
  names <- traverse (\_ -> newName "t") [1 .. count]

  let hlistT = mkHListT names
      hlistP = mkHListP names
      hlistE = mkHListE names

      tupleT = mkTupleT names
      tupleP = mkTupleP names
      tupleE = mkTupleE names

      morph = ConT (mkName "Morph")
      head  = AppT (AppT morph hlistT) tupleT

  pure $ InstanceD Nothing [] head
    [ FunD (mkName "fromTuple") [ Clause [ tupleP ] (NormalB hlistE) [] ]
    , FunD (mkName "toTuple")   [ Clause [ hlistP ] (NormalB tupleE) [] ]
    ]
