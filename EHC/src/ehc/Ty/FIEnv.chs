%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environmental info for subsumption (fitsIn).
This includes various environments like type synonym mapping, polarity info, etc.

Unfortunately it also contains functions which can not be imported directly because of
module circularity. In particular, a cycle exists by design because:
\begin{itemize}
\item Subsumption computes coercions,
\item which must be partially evaluated to determine how to compose such coercions.
\item Because of this eagerness already some tycore must be computed,
\item which includes also code for higher metalevels,
\item which is computed using subsumption.
\end{itemize}
If this is going to be avoided, the eagerness must be avoided, or use of subsumption for core generation.
%%]

%%[(4 hmtyinfer) module {%{EH}Ty.FIEnv} import({%{EH}Base.Common})
%%]

%%[(4 hmtyinfer) import({%{EH}Opts})
%%]

%%[(4 hmtyinfer) import(qualified Data.Set as Set)
%%]
%%[(4 hmtyinfer) import({%{EH}Gam})
%%]
%%[(8 hmtyinfer) import({%{EH}VarMp})
%%]
%%[(8 codegen) import({%{EH}AbstractCore})
%%]
%%[(8 hmtyinfer tycore) import(qualified {%{EH}TyCore.Full0} as C)
%%]
%%[(9 hmtyinfer) import({%{EH}Ty})
%%]
%%[(10 hmtyinfer) import(UHC.Util.Utils)
%%]

%%[(4 hmtyinfer) import({%{EH}Gam.AppSpineGam})
%%]
%%[(11 hmtyinfer) import({%{EH}Gam.TyGam})
%%]
%%[(17 hmtyinfer) import({%{EH}Gam.PolGam})
%%]
%%[(98 hmtyinfer) import({%{EH}Gam.DataGam})
%%]

For debug/trace:
%%[(4 hmtyinfer) import(UHC.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).FIEnv export(FIEnv(..))
data FIEnv
  =   FIEnv
        {   feAppSpineGam   :: !AppSpineGam			-- ty app spine info
%%[[8
        ,   feEHCOpts       :: !EHCOpts				-- compiler options
%%]]
%%[[(8 tycore)
        ,   feFIReqs        :: !FitsInRequires		-- functions required by fitsIn
%%]]
%%[[9
        ,   feDontBind      :: !TyVarIdS			-- inhibit type var binding for ...
        ,   fePredScope     :: !PredScope			-- the scope used by predicate resolution
%%]]
%%[[11
        ,   feTyGam         :: !TyGam				-- type environment, for type synonym expansion
%%]]
%%[[17
        ,   fePolGam        :: !PolGam				-- polarity environment, for co/contra variance
%%]]
%%[[98
        ,   feDataGam       :: !DataGam				-- datatype info, for tycore generation, for coercions
%%]]
%%[[99
        ,   feRange         :: !Range				-- position of source code from where subsumption is invoked
%%]]
        }
%%]

%%[(4 hmtyinfer) export(emptyFE)
emptyFE
  =   FIEnv
        {   feAppSpineGam   =   emptyGam
%%[[8
        ,   feEHCOpts       =   defaultEHCOpts
%%]]
%%[[(8 tycore)
        ,   feFIReqs        =	emptyFitsInRequires -- panic "FIEnv.feFIReqs: no way we can define this"
%%]]
%%[[9
        ,   feDontBind      =   Set.empty
        ,   fePredScope     =   initPredScope
%%]]
%%[[11
        ,   feTyGam         =   emptyGam
%%]]
%%[[17
        ,   fePolGam        =   emptyGam
%%]]
%%[[98
        ,   feDataGam       =   emptyGam
%%]]
%%[[99
        ,   feRange         =   emptyRange
%%]]
        }
%%]

%%[(4 hmtyinfer)
instance Show FIEnv where
  show _ = "FIEnv"
%%]

%%[(9 hmtyinfer)
instance PP FIEnv where
  pp e = "FIEnv"
         >#< (empty
%%[[11
             >-< pp (feTyGam e)
%%]]
             )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsInRequires
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer tycore) export(FitsInRequires(..))
data FitsInRequires
  =   FitsInRequires
        { fireqCSubstAppExpr 			:: C.CSubst -> C.Expr -> C.Expr
        , fireqCSubstAppSubst 			:: C.CSubst -> C.CSubst -> C.CSubst
        , fireqLRCoeForLamTyAppAsSubst	:: EHCOpts -> UID -> C.Ty -> C.LRCoe -> C.LRCoe -> (C.LRCoe,C.CSubst)
%%[[10
        , fireqCoeEvalOnAsSubst       	:: {- FIEnv -> -} UID -> C.Coe -> C.Expr -> (C.Expr,C.CSubst)
        , fireqLRCoeWipeWeaveAsSubst	:: EHCOpts -> UID -> VarMp -> C.LRCoe -> (C.Coe,C.CSubst)
%%]]
        }
%%]

%%[(8 hmtyinfer tycore) export(emptyFitsInRequires)
emptyFitsInRequires
  =   FitsInRequires
        { fireqCSubstAppExpr			= \_ a       -> a
        , fireqCSubstAppSubst			= \_ a       -> a
        , fireqLRCoeForLamTyAppAsSubst  = \_ _ _ _ _ -> (C.emptyLRCoe,emptyCSubst)
%%[[10
        , fireqCoeEvalOnAsSubst       	= \{- _ -} _ _ _   -> (C.Expr_Err "emptyFitsInRequires",emptyCSubst)
        , fireqLRCoeWipeWeaveAsSubst	= \_ _ _ _   -> (acoreCoeId,emptyCSubst)
%%]]
        }
%%]

