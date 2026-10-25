--# -path=.:../abstract:../common:../api:../prelude

concrete AllGer of AllGerAbs =
  LangGer - [DetNP, part_Prep, possess_Prep, that_Subj, UseCopula],
  IrregGer - [sein_V],
  ExtendGer - [DetNPMasc, DetNPFem, ProDrop, GenNP, PredVPS, ComplSlashPartLast, ProgrVPSlash]
--  ** open ExtraGer in {} ---- to force compilation
  , ExtraGer[
    Foc,FocObj,FocAdv,FocAP,UseFoc
      , TImpfSubj
      , moegen_VV
      , RelIP
      , Pass3V3
      , SuperlAdvAdj
      , WithoutToVP
      , FClause, VPass, AdvFor, FtoCl
      , EsVV, EsV2A
      , AdjNSg, AdjNPl, CardLN, OrdSgLN
      , ComplSlashRc, ComplSlashExt, CountNPExtRc, SelfNPExtRc
]
  ;
