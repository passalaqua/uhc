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

%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set, Data.Array, Data.Maybe, Data.List)
%%]

%%[(8 codegen) hs import(UHC.Util.Utils)
%%]

%%[(8 codegen) hs import(Control.Monad.State hiding (join), Control.Applicative)
%%]

-- debug only
%%[(8 codegen) hs import({%{EH}Base.Debug},UHC.Util.Pretty,{%{EH}AnaDomain.Pretty})
%%]

-- Avoid error on compiling for now:
%%[(8 codegen) hs export(a)
a = a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%[(8 codegen) hs
instance VarUpdatable RelevTy RVarMp where
  varUpd = relevtyAppVarLookup

instance VarExtractable RelevTy UID where
  varFreeSet = relevTyFtv
%%%]
