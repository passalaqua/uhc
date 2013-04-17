%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation information about the program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Globally maintained info about AST elements.
This can not reside in one particular phase as each phase gathers additional info to be added to a BindingInfo.
Hence its separate and globally available definition.

Currently the following is maintained:
\begin{itemize}
\item For Core:
  \begin{itemize}
  \item arity
  \item stack tracing
  \end{itemize}
\item For GrinCode:
\item For GrinByteCode:
  \begin{itemize}
  \item FunctionInfo index
  \end{itemize}
\end{itemize}
%%]

%%[(8 codegen) module {%{EH}LamInfo} import({%{EH}Base.Common})
%%]

-- Core, Ty
%%[(8 codegen) import({%{EH}AbstractCore})
%%]
%%[(8 codegen) import({%{EH}Ty})
%%]
%%[(8 codegen) import({%{EH}Core})
%%]
%%[(8 codegen coresysf) import(qualified {%{EH}Core.SysF.AsTy} as SysF)
%%]

-- Gam
%%[(8 codegen coresysf) import({%{EH}Gam},{%{EH}Gam.TyKiGam})
%%]

-- Analyses
%%[(8 codegen) import({%{EH}AnaDomain})
%%]
%%[(8 codegen) import(UHC.Util.Utils)
%%]

-- PP
%%[(8 codegen) import(UHC.Util.Pretty,{%{EH}AnaDomain.Pretty},{%{EH}Ty.Pretty})
%%]

-- Haskell stuff
%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[(50 codegen) import(Control.Monad, UHC.Util.Serialize)
%%]
%%[(50 codegen) import(Data.Typeable(Typeable,Typeable2), Data.Generics(Data))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Calling convention info for lambda expressions/CAFs: known function arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(StackTraceInfo(..))
data StackTraceInfo
  = StackTraceInfo_None
  | StackTraceInfo_HasStackTraceEquiv	HsName		-- has a stack traced equivalent
  | StackTraceInfo_IsStackTraceEquiv	HsName		-- is a stack traced equivalent
  deriving ( Show
%%[[50
           , Data, Typeable
%%]]
           )
%%]

%%[(93 codegen) hs export(FusionRole(..))
-- | The role a value takes in fusion
data FusionRole
  = FusionRole_Fuse			-- fuse this, i.e. inline, turned on by 'fuse f' for f
  | FusionRole_BuildLeft	-- role of g in 'convert g,h'
  | FusionRole_BuildRight	-- role of h in 'convert g,h'
  deriving ( Enum, Show
           , Data,Typeable
           )
%%]

%%[(93 codegen)
instance PP FusionRole where
  pp r = pp $ drop l $ show r
       where l = length "FusionRole_"
%%]

%%[(8 codegen) hs export(BindingInfoAsp(..))
-- | per aspect info
data BindingInfoAsp
  = BindingInfoAsp_RelevTy						-- relevance typing
      { libindaspRelevTy 		:: !RelevTy
      }
  | BindingInfoAsp_Ty							-- plain good old type
      { libindaspTy 			:: !Ty
      }
%%[[(8888 coresysf)
  | BindingInfoAsp_SysfTy						-- system F type
      { libindaspSysfTy 		:: !SysF.Ty
      }
%%]]
  | BindingInfoAsp_Core							-- actual Core, should go paired with Ty (?? maybe pair them directly)
      { libindaspMetaLev		:: !MetaLev
      , libindaspCore			:: !CExpr
      }
%%[[93
  | BindingInfoAsp_FusionRole						-- role in fusion
      { libindaspFusionRole 	:: !FusionRole
      }
%%]]
  deriving ( Show
%%[[50
           , Data, Typeable
%%]]
           )

type BindingInfoAspMp = Map.Map ACoreBindAspectKeyS BindingInfoAsp
%%]

%%[(8 codegen)
instance PP BindingInfoAsp where
  pp (BindingInfoAsp_RelevTy 	t) = "RTy"  >#< pp t
  pp (BindingInfoAsp_Ty      	t) = "Ty"   >#< pp t
  pp (BindingInfoAsp_Core    ml	c) = pp "Core" -- >#< pp c -- Core.Pretty uses BindingInfo, so module cycle...
%%[[93
  pp (BindingInfoAsp_FusionRole	r) = "Fuse" >#< pp r
%%]]
%%]

%%[(8 codegen) hs export(BindingInfo(..),emptyBindingInfo,emptyBindingInfo')
-- | per lambda implementation info
data BindingInfo
  = BindingInfo
      { laminfoArity				:: !Int					-- arity of function
      , laminfoStackTrace  			:: !StackTraceInfo			-- stacktrace
      , laminfoGrinByteCode			:: Maybe GrinByteCodeBindingInfo	-- GB specific info
      , laminfoBindAspMp			:: !BindingInfoAspMp			-- info organized per/keyed on aspect
      }
  deriving ( Show
%%[[50
           , Data, Typeable
%%]]
           )

emptyBindingInfo' :: BindingInfo
emptyBindingInfo' = BindingInfo 0 StackTraceInfo_None (Just emptyGrinByteCodeBindingInfo) Map.empty

emptyBindingInfo :: BindingInfo
emptyBindingInfo = BindingInfo 0 StackTraceInfo_None Nothing Map.empty
%%]

%%[(8 codegen)
instance PP BindingInfo where
  pp (BindingInfo ar _ bc m) = ppAssocL $ assocLMapKey ppACBaspKeyS $ Map.toList m
%%]

%%[(50 codegen) hs export(laminfo1stArgIsStackTrace)
laminfo1stArgIsStackTrace :: BindingInfo -> Bool
laminfo1stArgIsStackTrace (BindingInfo {laminfoStackTrace=StackTraceInfo_IsStackTraceEquiv _}) = True
laminfo1stArgIsStackTrace _                                                                = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LamMp, map for maintaining implementation info about functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

20100822 AD: Note: lamMpMergeInto and lamMpMergeFrom probably can be combined, but currently subtly differ in the flow of info.

%%[(8 codegen) hs export(LamMp,emptyLamMp)
type LamMp    = Map.Map HsName BindingInfo

emptyLamMp :: LamMp
emptyLamMp = Map.empty
%%]

%%[(8 codegen) hs export(lamMpUnionBindAspMp,lamMpUnionsBindAspMp)
-- union, including the aspect map, but arbitrary for the info itself
lamMpUnionBindAspMp :: LamMp -> LamMp -> LamMp
lamMpUnionBindAspMp = Map.unionWith (\i1 i2 -> i1 {laminfoBindAspMp = laminfoBindAspMp i1 `Map.union` laminfoBindAspMp i2})

lamMpUnionsBindAspMp :: [LamMp] -> LamMp
lamMpUnionsBindAspMp = foldr lamMpUnionBindAspMp Map.empty
%%]

%%[(8 codegen) hs export(lamMpMergeInto)
-- propagate from new (left) to prev (right), adding new entries if necessary, combining with mergeL2RInfo, finally combining/choosing maps with mergeL2RMp
lamMpMergeInto :: (BindingInfo -> BindingInfo -> BindingInfo) -> (LamMp -> LamMp -> LamMp) -> LamMp -> LamMp -> LamMp
lamMpMergeInto mergeL2RInfo mergeL2RMp newMp prevMp
  = mergeL2RMp newMpMerge prevMp
  where newMpMerge
          = Map.mapWithKey
              (\n i -> maybe i (mergeL2RInfo i) $ Map.lookup n prevMp
              ) newMp
%%]
lamMpMergeInto2 :: (BindingInfo -> BindingInfo -> BindingInfo) -> (LamMp -> LamMp -> LamMp) -> LamMp -> LamMp -> LamMp
lamMpMergeInto2 mergeL2RInfo mergeL2RMp newMp prevMp
  = mergeL2RMp newMpMerge prevMp
  where newMpMerge
          = lamMpMergeFrom
              Just
              (\(Just x) _ -> x)
              mergeL2RInfo
              mergeL2RMp
              emptyBindingInfo
              newMp prevMp

%%[(8 codegen) hs export(lamMpLookupAsp,lamMpLookupAsp2,lamMpLookupLam,lamMpLookupCaf)
lamMpLookupAsp :: HsName -> ACoreBindAspectKeyS -> LamMp -> Maybe BindingInfoAsp
lamMpLookupAsp n a m
  = fmap snd $ mapLookup2' laminfoBindAspMp n a m

lamMpLookupAsp2 :: ACoreBindRef -> LamMp -> Maybe BindingInfoAsp
lamMpLookupAsp2 (ACoreBindRef n (Just a)) m = lamMpLookupAsp n a m

lamMpLookupLam :: HsName -> LamMp -> Maybe Int
lamMpLookupLam n m
  = case Map.lookup n m of
      j@(Just (BindingInfo {laminfoArity=a})) | a > 0
        -> Just a
      _ -> Nothing

lamMpLookupCaf :: HsName -> LamMp -> Maybe Int
lamMpLookupCaf n m
  = case Map.lookup n m of
      j@(Just (BindingInfo {laminfoArity=a})) | a == 0
        -> Just a
      _ -> Nothing
%%]

%%[(8 codegen) hs export(lamMpFilterLam,lamMpFilterCaf)
lamMpFilterLam :: LamMp -> LamMp
lamMpFilterLam = Map.filter ((>0) . laminfoArity)

lamMpFilterCaf :: LamMp -> LamMp
lamMpFilterCaf = Map.filter ((==0) . laminfoArity)
%%]

%%[(8 codegen) hs export(lamMpMergeFrom)
-- | merge info from arbitrary map m into LamMp holding BindingInfo's
lamMpMergeFrom
  :: (BindingInfo -> Maybe x)				-- extract relevant info from a BindingInfo
     -> (Maybe x -> BindingInfo -> BindingInfo)		-- set the info
     -> (z -> x -> x)					-- merge info from new map and old info
     -> BindingInfo					-- default, empty BindingInfo
     -> Map.Map HsName z				-- arbitrary map holding info to merge
     -> LamMp -> LamMp
lamMpMergeFrom get set merge empty m lm
  = Map.foldrWithKey (\n z lm -> Map.alter (Just . upd z) n lm)
                    lm m
  where upd z (Just i) = set (Just (merge z $ maybe emptyExtra id $ get i)) i    
        upd z Nothing  = set (Just (merge z         emptyExtra           )) empty
        emptyExtra = panicJust "lamMpMergeFrom" $ get $ empty
%%]
-- | merge info from arbitrary map m into LamMp holding BindingInfo's
lamMpMergeFrom
  :: (BindingInfo -> Maybe x)				-- extract relevant info from a BindingInfo
     -> (Maybe x -> BindingInfo -> BindingInfo)		-- set the info
     -> (z -> x -> x)					-- merge info from new map and old info
     -> (LamMp -> LamMp -> LamMp)			-- merge the new into given mp
     -> BindingInfo					-- default, empty BindingInfo
     -> Map.Map HsName z				-- arbitrary map holding info to merge
     -> LamMp -> LamMp
lamMpMergeFrom get set merge mergeMp empty m lm
  = mergeMp
      (Map.mapWithKey (\n z -> upd z $ Map.lookup n lm) m)
      lm
  where upd z (Just i) = set (Just (merge z $ maybe emptyExtra id $ get i)) i    
        upd z Nothing  = set (Just (merge z         emptyExtra           )) empty
        emptyExtra = panicJust "lamMpMergeFrom" $ get $ empty

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function Info to be exported as part of BindingInfo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs export(GrinByteCodeBindingInfo(..),emptyGrinByteCodeBindingInfo)
data GrinByteCodeBindingInfo
  = GrinByteCodeBindingInfo
      { gblaminfoFuninfoKey		:: !Int					-- index into FunctionInfo table, to be referred to outside this module only
      }
  deriving
     ( Show
%%[[50
     , Typeable, Data
%%]]
     )

emptyGrinByteCodeBindingInfo :: GrinByteCodeBindingInfo
emptyGrinByteCodeBindingInfo = GrinByteCodeBindingInfo (-1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initial LamMp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(initLamMp)
initLamMp :: LamMp
%%[[(8 coresysf)
initLamMp
  = lamMpUnionsBindAspMp
      [ mk tkgiKi         metaLevTy [ (n,x) | (TyKiKey_Name n,x) <- gamToAssocL initTyKiGam]
      {-
      , mk (const kiStar) metaLevKi                                (gamToAssocL initKiGam)
      , mk (const kiStar) metaLevSo                                (gamToAssocL initSoGam)
      -}
      ]
  where mk get mlev l
          = lamMpUnionsBindAspMp [ mk1 (mlev + 1) n (SysF.ty2TySysf $ get t) | (n,t) <- l ]
          where mk1 l n e = Map.singleton n (emptyBindingInfo {laminfoBindAspMp = Map.fromList [(acbaspkeyDefaultSysfTy l, BindingInfoAsp_Core l e)]})
%%][8
initLamMp = emptyLamMp
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: ForceEval, Serializable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen grin) hs
instance Serialize GrinByteCodeBindingInfo where
  sput (GrinByteCodeBindingInfo a) = sput a
  sget = liftM  GrinByteCodeBindingInfo sget

%%[[93
instance Serialize FusionRole where
  sput = sputEnum8
  sget = sgetEnum8
%%]]

instance Serialize BindingInfoAsp where
  sput (BindingInfoAsp_RelevTy  	a) = sputWord8 0 >> sput a
  sput (BindingInfoAsp_Ty 		a) = sputWord8 1 >> sput a
  sput (BindingInfoAsp_Core 	a b) = sputWord8 2 >> sput a >> sput b
%%[[93
  sput (BindingInfoAsp_FusionRole 	a) = sputWord8 3 >> sput a
%%]]
%%[[(8888 coresysf)
  sput (BindingInfoAsp_SysfTy   	a) = sputWord8 4 >> sput a
%%]]
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  BindingInfoAsp_RelevTy  	sget
      1 -> liftM  BindingInfoAsp_Ty 		sget
      2 -> liftM2 BindingInfoAsp_Core 		sget sget
%%[[93
      3 -> liftM  BindingInfoAsp_FusionRole sget
%%]]
%%[[(8888 coresysf)
      4 -> liftM  BindingInfoAsp_SysfTy		sget
%%]]

instance Serialize BindingInfo where
  sput (BindingInfo a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 BindingInfo  sget sget sget sget

instance Serialize StackTraceInfo where
  sput (StackTraceInfo_None                ) = sputWord8 0
  sput (StackTraceInfo_HasStackTraceEquiv a) = sputWord8 1 >> sput a
  sput (StackTraceInfo_IsStackTraceEquiv  a) = sputWord8 2 >> sput a
  sget
    = do t <- sgetWord8
         case t of
           0 -> return StackTraceInfo_None
           1 -> liftM  StackTraceInfo_HasStackTraceEquiv sget
           2 -> liftM  StackTraceInfo_IsStackTraceEquiv  sget
%%]

