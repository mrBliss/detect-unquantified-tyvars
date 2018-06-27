{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module DetectUnquantifiedTyVars (plugin) where

-- base
import Data.Foldable (toList)
import Data.Coerce (coerce)

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- ghc
import qualified GHC
import qualified GhcPlugins as GHC
import qualified TcRnMonad as GHC

-- syb
import Data.Generics (Data, everythingWithContext, mkQ)


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
    mapM_ warnUnquantified $ unquantifiedTyVarsToList $ findUnquantifieds sig

warnUnquantified :: UnquantifiedTyVar -> GHC.TcM ()
warnUnquantified utv =
    GHC.addWarnAt GHC.NoReason (unquantifiedTyVarSrcSpan utv) $
    GHC.text "Type variable without explicit forall:" GHC.<+>
    GHC.ppr utv

-- * Looking for unquantified tyvars in the AST

findUnquantifieds :: Data a => a -> UnquantifiedTyVars
findUnquantifieds =
    everythingWithContext (mempty @Binders) (<>) $
    mkQ (\bndrs -> (mempty @UnquantifiedTyVars, bndrs)) processType
  where
    -- Whenever we encounter a type variable that does not occur in the set of
    -- quantified type variables, we return it as an unquantified one.
    --
    -- Whenever we encounter an explicit forall type, we add its binders to
    -- the set of quantified type variables.
    processType :: GHC.HsType GHC.GhcRn -> Binders
                -> (UnquantifiedTyVars, Binders)
    processType (GHC.HsTyVar _ _ lname@(GHC.L _ name)) bndrs
        | GHC.isTyVarName name
        , isUnquantified name bndrs
        = (unitUnquantifiedTyVars $ mkUnquantifiedTyVar lname, bndrs)
    processType (GHC.HsForAllTy _ lbndrs _) bndrs
        = let newBndrs = map (mkBinder . GHC.hsTyVarName . GHC.unLoc) lbndrs
          in  (mempty, addBinders newBndrs bndrs)
    processType _ bndrs = (mempty, bndrs)

-- | The name of a type variable that occurs as a binder in a forall.
newtype Binder = Binder (GHC.Name)
    deriving (Eq, Ord)

mkBinder :: GHC.Name -> Binder
mkBinder = Binder

-- | A collection of all the binders in scope.
newtype Binders = Binders GHC.NameSet
    -- A NameSet because we don't care for duplicates nor their ordering,
    -- remember that when the same type variable occurs in different scopes,
    -- it will not be equal (thanks to the renamer).
    deriving (Semigroup, Monoid)

addBinders :: [Binder] -> Binders -> Binders
addBinders = flip (coerce GHC.extendNameSetList)

isQuantified :: GHC.Name -> Binders -> Bool
isQuantified = coerce GHC.elemNameSet

isUnquantified :: GHC.Name -> Binders -> Bool
isUnquantified name = not . isQuantified name


-- | The name of a type variable that is not explicitly bound or quantified by
-- a forall. This includes the precise of the type variable.
newtype UnquantifiedTyVar = UTV { unUTV :: GHC.Located GHC.Name }

instance Eq UnquantifiedTyVar where
    (==) = coerce (GHC.eqLocated @GHC.Name)

instance Ord UnquantifiedTyVar where
    compare = coerce (GHC.cmpLocated @GHC.Name)

instance GHC.Outputable UnquantifiedTyVar where
    ppr = GHC.ppr . unquantifiedTyVarName

mkUnquantifiedTyVar :: GHC.Located GHC.Name -> UnquantifiedTyVar
mkUnquantifiedTyVar = UTV

unquantifiedTyVarName :: UnquantifiedTyVar -> GHC.Name
unquantifiedTyVarName = GHC.unLoc . unUTV

unquantifiedTyVarSrcSpan :: UnquantifiedTyVar -> GHC.SrcSpan
unquantifiedTyVarSrcSpan = GHC.getLoc . unUTV

-- | A collection of all the unquantified type variables.
--
-- A left-biased 'Set' is used, because when a type variable occurs multiple
-- times in a type signature, we only want to warn about the first occurrence.
newtype UnquantifiedTyVars = UTVs (Set UnquantifiedTyVar)
    deriving (Semigroup, Monoid)

unquantifiedTyVarsToList :: UnquantifiedTyVars -> [UnquantifiedTyVar]
unquantifiedTyVarsToList (UTVs ol) = toList ol

unitUnquantifiedTyVars :: UnquantifiedTyVar -> UnquantifiedTyVars
unitUnquantifiedTyVars = UTVs . Set.singleton
