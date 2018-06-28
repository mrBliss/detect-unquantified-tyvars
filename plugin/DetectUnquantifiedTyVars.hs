{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module DetectUnquantifiedTyVars (plugin) where

-- base
import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import Data.Foldable (toList)

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- ghc
import qualified GHC
import qualified GhcPlugins as GHC
import Maybes (orElse)
import qualified TcEnv as GHC
import qualified Bag as GHC
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
        GHC.XValBindsLR (GHC.NValBinds valBindGroups sigs) -> do
            -- For each signature,
            namesWithBndrs <- forM sigs $ \sig -> do
                -- Collect all binders and the unquantified type variables
                let (unquants, bndrs) = findUnquantifieds mempty sig
                -- Warn about the unquantified type variables
                mapM_ warnUnquantified $ unquantifiedTyVarsToList unquants
                -- Return a mapping from each name that has this signature to
                -- the binders of the signature
                return (zip (getTypeSigNames sig) (repeat bndrs))
            let nameToBndrs :: GHC.Name -> Binders
                nameToBndrs name = lookup name (concat namesWithBndrs) `orElse` mempty
                valBindToNames :: GHC.LHsBind GHC.GhcRn -> [GHC.Name]
                valBindToNames = GHC.collectHsBindBinders . GHC.unLoc
                valBinds :: [GHC.LHsBind GHC.GhcRn]
                valBinds = concatMap (GHC.bagToList . snd) valBindGroups
            -- For each value binding,
            forM_ valBinds $ \valBind -> do
                -- Get the binders corresponding with the name(s) of this
                -- value binding
                let valBindBndrs = foldMap nameToBndrs $ valBindToNames valBind
                    (unquants, _) = findUnquantifieds valBindBndrs valBind
                -- Warn about the unquantified type variables
                mapM_ warnUnquantified $ unquantifiedTyVarsToList unquants


getTypeSigNames :: GHC.LSig GHC.GhcRn -> [GHC.Name]
getTypeSigNames = GHC.nameSetElemsStable . GHC.getTypeSigNames . return

warnUnquantified :: UnquantifiedTyVar -> GHC.TcM ()
warnUnquantified utv =
    GHC.addWarnAt GHC.NoReason (unquantifiedTyVarSrcSpan utv) $
    GHC.text "Type variable without explicit forall:" GHC.<+>
    GHC.ppr utv

-- * Looking for unquantified tyvars in the AST

findUnquantifieds :: Data a
                  => Binders  -- ^ Type variables that are already bound
                  -> a        -- ^ The thing (type, signature, ...) to walk over
                  -> (UnquantifiedTyVars, Binders)
                     -- ^ The unquantified tyvars and all the binders
                     -- encountered along the way, excluding the initial bounders
findUnquantifieds initBndrs =
    everythingWithContext initBndrs (<>) $
    mkQ (\stateBndrs -> (mempty, stateBndrs)) processType
  where
    -- Whenever we encounter a type variable that does not occur in the set of
    -- quantified type variables, we return it as an unquantified one.
    --
    -- Whenever we encounter an explicit forall type, we add its binders to
    -- the set of quantified type variables.
    --
    -- We also collect all the new binders encountered along the way.
    processType :: GHC.HsType GHC.GhcRn -> Binders
                -> ( (UnquantifiedTyVars, Binders)
                     -- ^ The unquantified type variables and the new binders
                   , Binders)
                     -- ^ The binders that are in scope, including any new ones
    processType (GHC.HsTyVar _ _ lname@(GHC.L _ name)) stateBndrs
        | GHC.isTyVarName name
        , isUnquantified name stateBndrs
        = ((unitUnquantifiedTyVars $ mkUnquantifiedTyVar lname, mempty),
           stateBndrs)
    processType (GHC.HsForAllTy _ lbndrs _) stateBndrs
        = let newBndrs = mkBinders $
                         map (mkBinder . GHC.hsTyVarName . GHC.unLoc) lbndrs
          in  ((mempty, newBndrs), newBndrs <> stateBndrs)
    processType _ stateBndrs = (mempty, stateBndrs)

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

mkBinders :: [Binder] -> Binders
mkBinders = coerce GHC.mkNameSet

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
