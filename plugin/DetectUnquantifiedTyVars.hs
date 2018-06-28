{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DetectUnquantifiedTyVars (plugin) where

-- base
import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import Data.Foldable (toList)

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- mtl
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, local)
import Control.Monad.Trans (lift)

-- ghc
import qualified GHC
import qualified GhcPlugins as GHC
import Maybes (orElse)
import qualified Bag as GHC
import qualified TcRnMonad as GHC

-- syb
import Data.Generics (Data, everythingWithContext, mkQ)


-- | The plugin
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction = Just renamerPlugin
    }

renamerPlugin :: [GHC.CommandLineOption]
              -> GHC.ModSummary
              -> GHC.RenamedSource
              -> GHC.TcM ()
renamerPlugin _cmdLineOpts _modSummary (hsGroup, _, _, _) = do
    let GHC.HsGroup { GHC.hs_valds = valBinds
                    , GHC.hs_tyclds = tyClGroups } = hsGroup
    flip runReaderT mempty $ do
        checkHsValBinds valBinds
        mapM_ checkTyClGroup tyClGroups

-- | Class for letting GHC print a warning.
class Monad m => MonadWarning m where
    warn :: GHC.SrcSpan -> GHC.SDoc -> m ()

instance MonadWarning GHC.TcM where
    warn = GHC.addWarnAt GHC.NoReason

instance MonadWarning m => MonadWarning (ReaderT env m) where
    warn loc msg = lift (warn loc msg)

-- | Warn that the given type variable is unquantified.
warnUnquantified :: MonadWarning m => UnquantifiedTyVar -> m ()
warnUnquantified utv =
    warn (unquantifiedTyVarSrcSpan utv) $
    GHC.text "Type variable without explicit forall:" GHC.<+>
    GHC.ppr utv

-- | = Walking through the AST, checking for unquantified type variables
--
-- Unfortunately, we can't simply use the generic 'check' function, as the
-- scoping rules of type variables are more complex:
--
-- * Type variables quantified in a type signature also scope over the body,
--   which can contain pattern/expression signatures and type signatures for
--   local bindings.
--
-- * Local bindings can nest arbitrarly deep, so we will need some recursion.
--
-- * A single type signature can be given for multiple bindings, e.g.:
--
--   @
--       f, g :: forall a. a -> a
--       f x = (x :: a)
--       g (x :: a) = x
--   @
--
-- * In class declarations, the type arguments to the class are also in scope.
--
-- * In instance declarations, any type variables in the instance head are
--   also in scope.
--
-- For these reasons, we need to walk some parts of the AST manually instead
-- of generically.

-- | Generic function that warns about all unquantified type variables in
-- every @'GHC.HsType' 'GHC.GhcRn'@ that occurs in @a@ using
-- 'findUnquantifieds'.
check :: (MonadReader Binders m, MonadWarning m, Data a) => a -> m ()
check x = do
    bndrs <- ask
    let (unquants, _) = findUnquantifieds bndrs x
    mapM_ warnUnquantified $ unquantifiedTyVarsToList unquants

checkSigsAndHsBinds :: (MonadReader Binders m, MonadWarning m)
                    => [GHC.LSig GHC.GhcRn] -> [GHC.HsBind GHC.GhcRn] -> m ()
checkSigsAndHsBinds sigs binds = do
    bndrs0 <- ask
    -- For each signature,
    namesWithBndrs <- forM sigs $ \sig -> do
        -- Collect all binders and the unquantified type variables
        let (unquants, bndrs) = findUnquantifieds bndrs0 sig
        -- Warn about the unquantified type variables
        mapM_ warnUnquantified $ unquantifiedTyVarsToList unquants
        -- Return a mapping from each name that has this signature to
        -- the binders of the signature
        return (zip (getTypeSigNames sig) (repeat bndrs))

    let nameToBndrs :: GHC.Name -> Binders
        nameToBndrs name = lookup name (concat namesWithBndrs) `orElse` mempty

    forM_ binds $ \bind ->
        -- Get the binders corresponding with the name(s) of this value
        -- binding
        let bindBndrs = foldMap nameToBndrs $ bindToNames bind
        in  local (bindBndrs <>) $ checkHsBind bind
  where
    bindToNames :: GHC.HsBind GHC.GhcRn -> [GHC.Name]
    bindToNames = GHC.collectHsBindBinders

-- | Variant of 'TcEnv.getTypeSigNames' that:
-- * Takes one signature instead of a list.
-- * Does not ignore a 'GHC.ClassOpSig'.
-- * Returns a list of 'GHC.Name's instead of a 'GHC.NameSet'.
getTypeSigNames :: GHC.LSig GHC.GhcRn -> [GHC.Name]
getTypeSigNames lsig = map GHC.unLoc $ case GHC.unLoc lsig of
    GHC.TypeSig _ names _      -> names
    GHC.PatSynSig _ names _    -> names
    GHC.ClassOpSig _ _ names _ -> names
    _                          -> []

checkHsValBinds :: (MonadReader Binders m, MonadWarning m)
                => GHC.HsValBinds GHC.GhcRn -> m ()
checkHsValBinds (GHC.ValBinds {}) = return ()  -- Only before renaming
checkHsValBinds (GHC.XValBindsLR (GHC.NValBinds valBindGroups sigs)) =
    checkSigsAndHsBinds sigs binds
  where
    binds :: [GHC.HsBind GHC.GhcRn]
    binds = concatMap (map GHC.unLoc . GHC.bagToList . snd) valBindGroups

checkHsBind :: (MonadReader Binders m, MonadWarning m)
            => GHC.HsBind GHC.GhcRn -> m ()
checkHsBind (GHC.FunBind { GHC.fun_matches = fun_matches }) = do
    let GHC.MG { GHC.mg_alts = GHC.L _ lmatches } = fun_matches
        matches = map GHC.unLoc lmatches
    mapM_ check $ concatMap GHC.m_pats matches
    mapM_ checkGRHSs $ map GHC.m_grhss matches
checkHsBind (GHC.PatBind { GHC.pat_lhs = lpat, GHC.pat_rhs = grhss }) = do
    check lpat
    checkGRHSs grhss
checkHsBind (GHC.VarBind { GHC.var_rhs = expr }) = check expr
checkHsBind (GHC.PatSynBind _ (GHC.PSB { GHC.psb_def = lpat })) = check lpat
checkHsBind _ = return ()

checkGRHSs :: (MonadReader Binders m, MonadWarning m)
           => GHC.GRHSs GHC.GhcRn (GHC.LHsExpr GHC.GhcRn) -> m ()
checkGRHSs grhss = do
    mapM_ check $ GHC.grhssGRHSs grhss
    case GHC.unLoc $ GHC.grhssLocalBinds grhss of
        GHC.HsValBinds _ valBinds -> checkHsValBinds valBinds
        _ -> return ()

checkTyClGroup :: (MonadReader Binders m, MonadWarning m)
               => GHC.TyClGroup GHC.GhcRn -> m ()
checkTyClGroup (GHC.XTyClGroup {}) = return ()
checkTyClGroup (GHC.TyClGroup { GHC.group_tyclds = lTyClDecls
                              , GHC.group_instds = lInstDecls }) = do
    mapM_ (checkTyClDecl . GHC.unLoc) lTyClDecls
    mapM_ (checkInstDecl . GHC.unLoc) lInstDecls

checkTyClDecl :: (MonadReader Binders m, MonadWarning m)
              => GHC.TyClDecl GHC.GhcRn -> m ()
checkTyClDecl (GHC.ClassDecl { GHC.tcdTyVars = tyvars
                             , GHC.tcdSigs = sigs
                             , GHC.tcdMeths = lhsBinds }) = do
    let binders = mkBinders $ GHC.hsAllLTyVarNames tyvars
        binds = map GHC.unLoc $ GHC.bagToList lhsBinds
    local (binders <>) $ checkSigsAndHsBinds sigs binds
checkTyClDecl _ = return ()

checkInstDecl :: (MonadReader Binders m, MonadWarning m)
              => GHC.InstDecl GHC.GhcRn -> m ()
checkInstDecl (GHC.ClsInstD _
               (GHC.ClsInstDecl { GHC.cid_poly_ty = sigType
                                , GHC.cid_binds = lhsBinds
                                , GHC.cid_sigs = sigs })) = do
    let tyvars = case sigType of
            GHC.HsIB { GHC.hsib_ext = ibrn } -> GHC.hsib_vars ibrn
            _   -> mempty
        binders = mkBinders tyvars
        binds = map GHC.unLoc $ GHC.bagToList lhsBinds
    local (binders <>) $ checkSigsAndHsBinds sigs binds
checkInstDecl _ = return ()


-- | Generically find unquantified type variables in some piece of AST that
-- contains @'GHC.HsType' 'GHC.GhcRn'@.
findUnquantifieds :: Data a
                  => Binders  -- ^ Type variables that are already bound/in
                              -- scope.
                  -> a        -- ^ The thing (type, signature, ...) to walk
                              -- over
                  -> (UnquantifiedTyVars, Binders)
                     -- ^ The unquantified tyvars and all the binders
                     -- encountered along the way, excluding the initial
                     -- binders
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
        = let newBndrs = mkBinders $ map (GHC.hsTyVarName . GHC.unLoc) lbndrs
          in  ((mempty, newBndrs), newBndrs <> stateBndrs)
    processType _ stateBndrs = (mempty, stateBndrs)

-- | A collection of all the binders in scope.
newtype Binders = Binders GHC.NameSet
    -- A NameSet because we don't care for duplicates nor their ordering,
    -- remember that when the same type variable occurs in different scopes,
    -- it will not be equal (thanks to the renamer).
    deriving (Semigroup, Monoid)

-- | Useful for debugging
instance GHC.Outputable Binders where
    ppr (Binders names) = GHC.ppr (GHC.nameSetElemsStable names)

mkBinders :: [GHC.Name] -> Binders
mkBinders = coerce GHC.mkNameSet

isQuantified :: GHC.Name -> Binders -> Bool
isQuantified = coerce GHC.elemNameSet

isUnquantified :: GHC.Name -> Binders -> Bool
isUnquantified name = not . isQuantified name


-- | The name of a type variable that is not explicitly bound or quantified by
-- a forall. This includes the location of the type variable.
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
