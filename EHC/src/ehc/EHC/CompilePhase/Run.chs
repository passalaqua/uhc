%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile: running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) module {%{EH}EHC.CompilePhase.Run}
%%]

%%[(8 corerun) import({%{EH}EHC.Common})
%%]
%%[(8 corerun) import({%{EH}EHC.CompileUnit})
%%]
%%[(8 corerun) import({%{EH}EHC.CompileRun})
%%]

%%[(8 corerun) import(Data.Maybe)
%%]
%%[(8 corerun) import(Control.Monad.State)
%%]
%%[(8 corerun) import(Control.Exception)
%%]

-- Acccess to Core
%%[(8 corerun) import({%{EH}EHC.CompilePhase.Parsers})
%%]

-- CoreRun
%%[(8 corerun) import({%{EH}Core.ToCoreRun}, {%{EH}CoreRun})
%%]
%%[(8888 corerun) import({%{EH}CoreRun.Pretty})
%%]

-- Running CoreRun
%%[(8 corerun) import({%{EH}CoreRun.Run})
%%]
%%[(8888 corerun) import({%{EH}CoreRun.Run.Val.RunImplStk} as RI)
%%]
%%[(8 corerun) import({%{EH}CoreRun.Run.Val.RunExplStk} as RE)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) export(cpRunCoreRun)
-- | Run CoreRun.
-- TBD: fix dependence on whole program linked
cpRunCoreRun :: HsName -> EHCompilePhase ()
cpRunCoreRun modNm = do
    cr <- get
    let (ecu,_,opts,_) = crBaseInfo modNm cr
        mbCore = ecuMbCore ecu
%%[[8
        (mainMod,impModL) = (modNm,[])
%%][50
%%]]
    cpMsg modNm VerboseNormal "Run Core"
    when (isJust mbCore) $ do
      let mod = cmod2CoreRun $ fromJust mbCore
      res <- liftIO $ catch
        (runCoreRun opts [] mod $ cmodRun opts mod)
        (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun: " ++ show e))
      either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
             (liftIO . putStrLn . show . pp)
%%][100
             (\_ -> return ())
%%]]
             res
%%]

%%[(8 corerun) export(cpRunCoreRun2)
-- | Run CoreRun.
-- 20150130: TBD: does not work yet
cpRunCoreRun2 :: HsName -> EHCompilePhase ()
cpRunCoreRun2 modNm = do
    cr <- get
    let (ecu,_,opts,_) = crBaseInfo modNm cr
        mbCore = ecuMbCore ecu
%%[[8
    let (impModL, mainMod) = ([], cmod2CoreRun $ fromJust mbCore)
%%][50
    (impModL, mainMod) <- fmap (fromJust . initlast) $
      case crPartitionMainAndImported cr $ map head $ crCompileOrder cr of
        (_, impl) -> do
          cores <- forM (impl ++ [modNm]) cpGetPrevCore
          return $ flip evalState emptyNm2RefMp $ do
            forM (zip cores [0..]) $ \(cr,modnr) -> do
              prevNm2Ref <- get
              let (m,nm2ref) = cmod2CoreRun' opts modnr prevNm2Ref cr
              put $ nm2refUnion nm2ref prevNm2Ref
              return m
%%]]
    cpMsg modNm VerboseNormal "Run Core"
    res <- liftIO $ catch
      (runCoreRun opts impModL mainMod $ cmodRun opts mainMod)
      (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "cpRunCoreRun: " ++ show e))
    either (\e -> cpSetLimitErrsWhen 1 "Run Core(Run) errors" [e])
%%[[8
           (liftIO . putStrLn . show . pp)
%%][100
           (\_ -> return ())
%%]]
           res
%%]


