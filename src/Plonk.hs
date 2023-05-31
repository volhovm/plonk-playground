{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Plonk task

module Plonk where

import           Prelude hiding ((<*>))

import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.Random (MonadRandom(..), Random(..))
import           Control.Monad.State (MonadState(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List ((!!))

import           Field


----------------------------------------------------------------------------
-- Basic algebra
----------------------------------------------------------------------------

type MyField = Z 1301
instance PrimeNat 1301

class Field f => PField f where
    w :: f
    k :: Integer
    n :: Integer
    n = 2 ^ (k @f)


evalPoly :: (Field f) => Poly f -> f -> f
evalPoly = undefined

allW :: forall f. PField f => [f]
allW = map (\i -> (w @f) <^> i) [0..(n @f)-1]

interpolate :: forall f. PField f => [f] -> Poly f
interpolate input =
    foldr1 (<*>) $
    map (\(c,i) -> [c] <*> lagrange i) $ input `zip` [0..n @f-1]
  where
    lagrange i = lagTop i `undefined` lagBot i --  division
    lagTop _i = undefined -- \prod_j (X - w^j), j != i
    lagBot _i = undefined -- \prod_j (w^i - w^j), j != i


newtype Perm
  = Perm [Integer]

permute :: [a] -> Perm -> [a]
permute x (Perm perm) = map (\i -> x !! (fromInteger i)) perm

----------------------------------------------------------------------------
-- Crypto
----------------------------------------------------------------------------

hash :: Field f => a -> f
hash = undefined

class (PField f, AGroup c) => Curve c f where

class PField f => PCS pcs f where
    type PCSCom pcs f
    type PCSProof pcs f

    pcsCommit :: Poly f -> PCSCom pcs f
    pcsOpen :: Poly f -> f -> PCSProof pcs f
    -- ^ [f] are input values x for P(x)
    pcsVerify :: PCSCom pcs f -> f -> f -> PCSProof pcs f -> Bool
    -- ^ (x,t) where t is eval t = P(x)


----------------------------------------------------------------------------
-- Arithmetisation
----------------------------------------------------------------------------

-- q_A x_a + q_B x_b + q_AB x_a x_b + q_C x_c + q_const = 0


data Proxy f

data Circuit f = Circuit
    { qA     :: [Integer]
    , qB     :: [Integer]
    , qAB    :: [Integer]
    , qC     :: [Integer]
    , qConst :: [Integer]
    , permA  :: Perm
    , permB  :: Perm
    , permC  :: Perm
    , circProxy :: Proxy f
    }


data Assignment f = Assignment
    { unAssignment :: [f]
    }


checkCircuit :: PField f => Circuit f -> Assignment f -> Bool
checkCircuit _circ _assignment = do
    -- check equation
    undefined

newtype Instance f
  = Instance { unInstance :: [f] }
newtype Witness f
  = Witness { unWitness :: [f] }

-- checkCircuit (reprLang @c) (Assignment $ (unInstance x) ++ (unWitness (evalLanguage x))) = true
class PField f => Language f l where
    reprLang :: Circuit f
    evalLanguage :: Instance f -> Witness f


----------------------------------------------------------------------------
-- Top-level interface
----------------------------------------------------------------------------

type ExecM m = (MonadRandom m,
                MonadError String m)


data PlonkSetup f pcs

data PlonkProof f pcs = PlonkProof {
                                   }

setup :: (PField f, PCS pcs f) => PlonkSetup f pcs
setup = undefined

prove ::
    forall f pcs m.
       (ExecM m, PField f, PCS pcs f)
    => PlonkSetup f pcs
    -> Circuit f
    -> Instance f
    -> Witness f
    -> m (PlonkProof f pcs)
prove _crs circ@Circuit{..} inst wit = do
    let x = unInstance inst
    let x_a = permute x permA
    let x_b = permute x permB
    let x_c = permute x permC

    let toField :: Integer -> f
        toField = (`times` f1 @f)

    let f_x_a = interpolate x_a
    let f_x_b = interpolate x_b
    let f_x_c = interpolate x_c
    let f_qA = interpolate $ map toField qA
    let f_qB = interpolate $ map toField qB
    let f_qAB = interpolate $ map toField qAB
    let f_qC = interpolate $ map toField qC
    let f_qConst = interpolate $ map toField qConst

    -- x^n - 1
    let z_H:: Poly f = ([toField 1,toField 0] <^> n @f) <-> [toField 1]


    ---- Circuit evaluation

    let ch1 = hash @f (circ, inst, wit)
    let f_x_a_eval = evalPoly f_x_a ch1
    let f_x_b_eval = evalPoly f_x_b ch1
    let f_x_c_eval = evalPoly f_x_c ch1

    let circuit_poly :: Poly f =
            [f_x_a_eval] <*> f_qA <+>
            [f_x_b_eval] <*> f_qB <+>
            [f_x_a_eval <*> f_x_b_eval] <*> f_qAB <+>
            [f_x_c_eval] <*> f_qC <+>
            f_qConst


    ---- Permutation argument
    -- f_x_a, f_x_b, f_x_c are permutations of each other according to x_a,...

    let z_perm :: Poly f = undefined f_x_a f_x_b f_x_c

    undefined



verify ::
       (MonadError String m, PField f, PCS pcs f)
    => PlonkSetup f pcs
    -> Circuit f
    -> Instance f
    -> PlonkProof f pcs
    -> m Bool
verify _crs _circ _inst _proof = undefined


----------------------------------------------------------------------------
-- Concrete/tests
----------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "bla"
