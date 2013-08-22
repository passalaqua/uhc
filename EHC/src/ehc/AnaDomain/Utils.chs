%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AnaDomain utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}AnaDomain.Utils} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(8 codegen) hs import({%{EH}AnaDomain},{%{EH}AnaDomain.Trf.Subst},{%{EH}AnaDomain.Ftv})
%%]

%%[(8 codegen) hs import({%{EH}Gam.DataGam})
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set, Data.Array, Data.Maybe, Data.List, Control.Arrow)
%%]

%%[(8 codegen) hs import(UHC.Util.Utils)
%%]

%%[(8 codegen) hs import(Control.Monad.State hiding (join), Control.Applicative)
%%]

-- debug only
%%[(8 codegen) hs import({%{EH}Base.Debug},UHC.Util.Pretty,{%{EH}AnaDomain.Pretty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
instance VarUpdatable RelTyScheme RVarMp where
  varUpd = relTySchAppVarLookup

instance VarUpdatable (RelTyScheme,Relev) RVarMp where
  varUpd rvmap = first (varUpd rvmap)

instance VarExtractable RelTyScheme UID where
  varFreeSet = tyFv
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generalization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
genTySch :: UIDS -> RelTyScheme -> RelTyScheme
genTySch ftvE ty@(RelTyScheme_Ty a b g tyIn) = RelTyScheme_Ty a' b' g' tyIn
  where
    a' = (tyFv ty `Set.difference` ftvE) `Set.union` a
    b' = relFv ty `Set.union` b
    g' = appFv ty `Set.union` g
%%]