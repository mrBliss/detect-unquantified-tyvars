{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module DetectUnquantifiedTyVars (plugin) where

-- base
import Data.Coerce (coerce)

-- ghc
import qualified GHC
import qualified GhcPlugins as GHC
import qualified TcRnMonad as GHC

-- syb
import Data.Generics (Data, everything, everythingWithContext, mkQ)

-- * The plugin
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction = Just renamerPlugin
    }

renamerPlugin :: [GHC.CommandLineOption]
              -> GHC.ModSummary
              -> GHC.RenamedSource
              -> GHC.TcM ()
renamerPlugin _cmdLineOpts _modSummary (hsGroup, _, _, _) = do
    let GHC.HsGroup { GHC.hs_valds = hs_valds
                    , GHC.hs_tyclds = hs_tyclds } = hsGroup
    -- TODO hs_tyclds :: [TyClGroup GhcRn]
    case hs_valds of
        GHC.ValBinds {} -> return ()
        GHC.XValBindsLR (GHC.NValBinds _val_binds sigs) ->
            mapM_ (checkSignature . GHC.unLoc) sigs
        -- TODO check pattern/expression/local signatures in val_binds

checkSignature :: GHC.Sig GHC.GhcRn -> GHC.TcM ()
checkSignature sig =
    mapM_ warnUnquantified $ getTyVarNames $ findUnquantifieds sig

warnUnquantified :: TyVarName -> GHC.TcM ()
warnUnquantified tvn =
    GHC.addWarnAt GHC.NoReason (tyVarNameSrcSpan tvn) $
    GHC.text "Type variable without explicit forall:" GHC.<+>
    GHC.ppr tvn

-- * Looking for unquantified tyvars in the AST

findUnquantifieds :: Data a => a -> TyVarNames 'Unquantified
findUnquantifieds =
    everythingWithContext (emptyTyVarNames @'Quantified) (<>) $
    mkQ (\quants -> (emptyTyVarNames @'Unquantified, quants)) processType
  where
    -- Whenever we encounter a type variable that does not occur in the set of
    -- quantified type variables, we return it as an unquantified one.
    --
    -- Whenever we encounter an explicit forall type, we add its binders to
    -- the set of quantified type variables.
    processType :: GHC.HsType GHC.GhcRn -> TyVarNames 'Quantified
                -> (TyVarNames 'Unquantified, TyVarNames 'Quantified)
    processType ty quants
        | Just name <- hsTypeTyVarName ty
        , isUnquantified name quants
        = (unquantified name, quants)
    processType (GHC.HsForAllTy _ lbndrs _) quants
        = let newQuants = map (mkTyVarName . fmap GHC.hsTyVarName) lbndrs
          in  (mempty, addQuantifieds newQuants quants)
    processType _ quants = (mempty, quants)

hsTypeTyVarName :: GHC.HsType GHC.GhcRn -> Maybe TyVarName
hsTypeTyVarName (GHC.HsTyVar _ _ lname@(GHC.L _ name))
    | GHC.isTyVarName name = Just (mkTyVarName lname)
hsTypeTyVarName _ = Nothing


newtype TyVarName = TVN { unTVN :: GHC.Located GHC.Name }

instance Eq TyVarName where
    (==) = coerce (GHC.eqLocated @GHC.Name)

instance Ord TyVarName where
    compare = coerce (GHC.cmpLocated @GHC.Name)

instance GHC.Outputable TyVarName where
    ppr = GHC.ppr . GHC.unLoc . unTVN

instance GHC.Uniquable TyVarName where
    getUnique = GHC.getUnique . GHC.unLoc . unTVN

mkTyVarName :: GHC.Located GHC.Name -> TyVarName
mkTyVarName = TVN

tyVarNameSrcSpan :: TyVarName -> GHC.SrcSpan
tyVarNameSrcSpan = GHC.getLoc . unTVN


-- * Tracking quantified and unquantified type variables

-- | A set of type variable names.
--
-- While walking the AST, we need to keep track of a set of quantified tyvars,
-- and also return a set of unquantified tyvars. To avoid confusing these
-- sets, we use 'Quantified' as type index, so the type checker can help us.
newtype TyVarNames (q :: Quantified) = TyVarNames (GHC.UniqSet TyVarName)
    deriving (Semigroup, Monoid)

data Quantified = Quantified | Unquantified

-- | Create an empty 'TyVarNames'.
--
-- Use this in combination with a type application of the desired @q@ instead
-- of 'mempty' to avoid mixing @q@s.
emptyTyVarNames :: TyVarNames q
emptyTyVarNames = TyVarNames GHC.emptyUniqSet

unquantified :: TyVarName -> TyVarNames 'Unquantified
unquantified = TyVarNames . GHC.unitUniqSet

addQuantifieds :: [TyVarName] -> TyVarNames q -> TyVarNames q
addQuantifieds = flip (coerce (GHC.addListToUniqSet @TyVarName))

isQuantified :: TyVarName -> TyVarNames 'Quantified -> Bool
isQuantified = coerce (GHC.elementOfUniqSet @TyVarName)

isUnquantified :: TyVarName -> TyVarNames 'Quantified -> Bool
isUnquantified name = not . isQuantified name

getTyVarNames :: TyVarNames q -> [TyVarName]
getTyVarNames = coerce (GHC.nonDetEltsUniqSet @TyVarName)

-- TODO problem: the warning is for the second occurrence of a instead of
-- the first. Either add all occurrences of the same variable, or make sure
-- the one that occurs first is retained
--
--   |
--   | const :: a -> b -> a
--   |               ^
--   |
--   | const :: a -> b -> a
--   |                    ^
