{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Playground for plonk.

module PlonkOld where

import           Prelude

import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.Random (MonadRandom(..), Random(..))
import           Control.Monad.State (MonadState(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Field

----------------------------------------------------------------------------
-- Curves and pairings
----------------------------------------------------------------------------

class (FField (CurveField c), AGroup c) => Curve c where
    type CurveField c


class (Curve (PairCurve1 pc), Curve (PairCurve2 pc), Curve (PairCurveT pc)) => PairingCurves pc where
    type PairCurve1 pc
    type PairCurve2 pc
    type PairCurveT pc
    pairing :: PairCurve1 pc -> PairCurve2 pc -> PairCurveT pc


----------------------------------------------------------------------------
-- PCS
----------------------------------------------------------------------------
class FField f => PCS pcs f where
    type PCSCom pcs f
    type PCSProof pcs f
    pcsCommit :: Poly f -> PCSCom pcs f
    pcsOpen :: PCSCom pcs f -> f -> PCSProof pcs f
    pcsVerify :: PCSCom pcs f -> PCSProof pcs f -> f -> Bool

data KZG pc

-- FIX: we don't need Ffield f here! Or Curve c1!
instance (PairingCurves pc, Curve c1, c1 ~ PairCurve1 pc, f ~ CurveField c1) => PCS (KZG pc) f where
    type PCSCom (KZG pc) f = f
    type PCSProof (KZG pc) f = f
    pcsCommit = undefined
    pcsOpen = undefined
    pcsVerify = undefined

----------------------------------------------------------------------------
-- PIOP
----------------------------------------------------------------------------


----------------------------------------------------------------------------
-- Arithmetization
----------------------------------------------------------------------------

class Circuit c where
    synthesize :: c -> Int

data MyCircuit


----------------------------------------------------------------------------
-- Top-level
----------------------------------------------------------------------------

type ContextM m = (MonadError String m,
                   MonadRandom m)

prove1 :: forall m f. (ContextM m, Field f, Random f) => m (Poly f)
prove1 = do
    when False $ throwError "mda"
    _x :: f <- getRandom
    undefined

data Nested = Nested
    { _nsSmth :: Either Bool Integer
    }
    deriving (Eq, Show)

makeLenses ''Nested

data BuilderState = BuilderState
    { _bsSize   :: Integer
    , _bsMaybe  :: Maybe Integer
    , _bsEither :: Either Integer Bool
    , _bsNested :: Maybe Nested
    , _bsList   :: [Integer]
    , _bsMap    :: Map String Integer
    }
    deriving (Eq, Show)

makeLenses ''BuilderState

type BuilderM m = (MonadError String m,
                   MonadRandom m,
                   MonadState BuilderState m
                   )

testBuild :: BuilderM m => m ()
testBuild = do
    bsSize .= 5
    bsSize %= (+1)
    bsSize += 1
    bsEither . _Left .= 5
    bsEither . _Right .= True
    bsMaybe . _Just .= 5
    bsNested . _Just . nsSmth . _Left .= True
    _size <- use bsSize
    _e :: Integer <- (!! 5) <$> use bsList
    bsList . ix 5 .= 5
    _e :: Maybe Integer <- preuse $ bsList . ix 5
    _e :: Maybe Integer <- use $ bsMap . at "bla"
    bsMap . at "bla" .= Just 5
    undefined

newtype Instance
  = Instance { getInstanceReallyLong :: [Integer] }
    deriving (Eq, Show)

data Witness = Witness
    { getWitness :: [Integer]
    , getMem     :: Bool
    }
    deriving (Eq, Show)

data Proof

prover :: Circuit c => c -> Instance -> Witness -> Maybe Proof
prover = undefined

verifier :: Circuit c => c -> Instance -> Proof -> Bool
verifier = undefined

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

type MyField = Z 1301
instance PrimeNat 1301
--type MyField = Z 1000000103
--instance PrimeNat 1000000103

data MyCurve1 = MyCurve1
    { x :: MyField
    , y :: MyField
    }
    deriving (Eq, Show)

instance AGroup MyCurve1 where
    f0 = undefined
    (<+>) = undefined
    fneg = undefined

instance Curve MyCurve1 where
    type CurveField MyCurve1 = MyField

main :: IO ()
main = do
    let p1 :: Poly MyField = ([1,0] <^> 6) <+> [-1] -- x^6 - 1
    putStrLn $ prettyPoly p1
    let p2 :: Poly MyField = Poly $ [2,3,-1]
    let k :: MyField = 5
    let p3 = unZ k `times` (p1 <+> p2)
    putStrLn $ prettyPoly p3
    let (p4q,p4r) = p3 </> p2
    putStrLn $ show p4q
    --putStrLn $ show $ p4r == zero
    --putStrLn $ prettyPoly p4q
