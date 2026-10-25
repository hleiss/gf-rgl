--# -path=.:../abstract:../common:../prelude

abstract AllGerAbs = 
  Lang - [DetNP, part_Prep, possess_Prep, that_Subj, UseCopula],
  IrregGerAbs,
  Extend - [DetNPMasc, DetNPFem, ProDrop, GenNP]
--  ** open ExtraGerAbs in {} ;
  , ExtraGerAbs -- restricted to what is not defined in Extend:
  [ Foc,FocObj,FocAdv,FocAP,UseFoc
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
  ] ;

