%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substituting holes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) hs module {%{EH}TyCore.Subst} import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map,UHC.Util.Pretty,UHC.Util.Utils,{%{EH}Opts},{%{EH}Base.Common},{%{EH}VarMp})
%%]

%%[(8 codegen tycore) hs import({%{EH}AbstractCore})
%%]

%%[(8 codegen tycore) hs import({%{EH}TyCore.Base},{%{EH}TyCore.Pretty},{%{EH}TyCore.Coercion})
%%]

%%[(8 codegen tycore) hs import(qualified {%{EH}Ty} as T)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) hs export(CSubst,CSubstInfo)
type CSubstInfo = CSubstInfo' Expr MetaVal ValBind ValBind Ty
type CSubst     = CSubst'     Expr MetaVal ValBind ValBind Ty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution as class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) hs export(CSubstitutable(..))
infixr `cSubstApp`

class CSubstitutable a where
  cSubstApp :: CSubst -> a -> a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst combination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

On CSubst, merges only, application is postponed

%%[(8888 codegen) hs export(cSubstAppSubst)
cSubstAppSubst :: CSubst -> CSubst -> CSubst
cSubstAppSubst = Map.union
%%]
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion: canceling (wiping) & combining (weaving)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LRCoe coercion for lamda
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) hs export(lrcoeWipeWeaveAsSubst)
lrcoeWipeWeaveAsSubst :: EHCOpts -> UID -> VarMp -> LRCoe -> (Coe,CSubst)
lrcoeWipeWeaveAsSubst opts uniq cnstr (LRCoe LRCoeId _ _) = (acoreCoeId,emptyCSubst)
lrcoeWipeWeaveAsSubst opts uniq cnstr lrcoe               = coeWipeWeaveAsSubst opts uniq cnstr (lrcoeLeftL lrcoe) (lrcoeRightL lrcoe)
%%]


