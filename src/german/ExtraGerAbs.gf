--# -path=.:../abstract:../common:../api:../prelude

abstract ExtraGerAbs = Extra [
  VPI,ListVPI,BaseVPI,ConsVPI,MkVPI,ComplVPIVV,ConjVPI,ClSlash,RCl,
  VPS,ListVPS,BaseVPS,ConsVPS,ConjVPS,MkVPS,PredVPS,EmptyRelSlash,
  VPSlash, PassVPSlash, PassAgentVPSlash, CompIQuant, PastPartAP, PastPartAgentAP,
  -- Temp,Tense,Pol,S,NP,VV,VP,Conj,IAdv,IQuant,IComp,ICompAP,IAdvAdv,Adv,AP,
  -- Cl, Num, CN, Utt, Predet,
  Foc,FocObj,FocAdv,FocAP,UseFoc,
    RNP,ReflRNP,ReflPron,ReflPoss,PredetRNP
    ,RNPList,ConjRNP,Base_rr_RNP,Base_nr_RNP,Base_rn_RNP,Cons_rr_RNP,Cons_nr_RNP
    ,DetNPMAsc,DetNPFem
  ] ** {
  flags coding=utf8;
  
  fun
    PPzuAdv   : CN -> Adv ;  -- zum Lied, zur Flasche
    TImpfSubj : Tense ;      -- ich möchte...   --# notpresent

    moegen_VV : VV ;         -- ich mag/möchte singen

    DetNPMasc, DetNPFem : Det -> NP ;

    -- Interrogative noun phrase with relative clause
    RelIP : IP -> RS -> IP ;  -- wer, der gesund ist (, schläft tagsüber?)

    -- Bekommen-passiv: the indirect object of v3 is the subject of (Pass3V2 v3)
    Pass3V3 : V3 -> VPSlash ; -- wir bekommen den Beweis erklärt

    -- Adverb fro predicative adjective in superlative degree, e.g. "am besten"
    SuperlAdvAdj : A -> Adv ;
    -- Adverb from infinitive (in addition to Extend.InOrderToVP)
    WithoutToVP : VP -> Adv ;

  cat
    FClause ; -- formal clause

  fun
  	VPass : V -> FClause ;   -- (es) wird getanzt
  	AdvFor : Adv -> FClause -> FClause ; -- es wird heute gelacht - addition of adverbs
  	FtoCl : FClause -> Cl ;  -- embedding FClause within the RGL, to allow generation of S, Utt, etc.

	EsVV : VV -> VP -> VP ; -- ich genieße es zu schlafen
 	EsV2A : V2A -> AP -> S -> VP ; -- ich finde es schön, dass ...

    -- Quaternary verbs (too expensive, expls in gf-rgl/tests/german/TestLexiconGerAbs)
  cat V4 ;
  fun
    -- Some specific constructions in addition to AdjLN : AP -> LN -> LN
    AdjNSg : A -> N -> LN ;       -- (the) Black Sea, (das) Schwarze Meer
    AdjNPl : A -> N -> LN ;       -- (the) Rocky Mountains, (die) Blauen Berge
    CardLN : Card -> N -> LN ;    -- (the) Five Islands, (die) Drei Zinnen, non-loc: Drei Musketiere
    OrdSgLN : Ord -> N -> LN ;    -- (the) Second Cataract, non-loc: Zweite Weltkrieg

    -- Interrogative verb phrase IVP (instead we use QVP for double questions)

    -- Interrogatve correlate for sentential object
    -- ICorVS : VS -> QVP ;          -- woran glauben
    -- PredIQVP : QVP -> NP -> QCl ; -- woran glauben die Kinder

    -- further constructions using RNP, declared in abstract/Extend.gf:

    AdvRNP : NP -> Prep -> RNP -> RNP ;   -- a dispute with his wife
    AdvRVP : VP -> Prep -> RNP -> VP ;    -- lectured about her travels
    AdvRAP : AP -> Prep -> RNP -> AP ;    -- adamant in his refusal

    ReflA2RNP : A2 -> RNP -> AP ;         -- indifferent to their surroundings
                                               -- NOTE: generalizes ReflA2

    PossPronRNP : Pron -> Num -> CN -> RNP -> NP ; -- his abandonment of his wife and children

    -- Extraction of relative and object clause of object np behind infinite part of verb
    ComplSlashRc  : VPSlash -> NP -> VP ;  -- vfin ++ (np - rc) ++ vinf ++ np.rc
    ComplSlashExt : VPSlash -> NP -> VP ;  -- vfin ++ (np - rc,ext) ++ vinf ++ np.ext+np.rc

    CountNPExtRc : Det -> NP -> NP ;  -- Lift np.ext and np.rc
    SelfNPExtRc : NP -> NP ;          -- Lift np.ext and np.rc
    -- These rules already lift np.rc and rn.ext to the result:
    -- PredetNP : Predet -> NP -> NP ;
    -- PPartNP : NP -> V2 -> NP ;
    -- AdvNP : NP -> Adv -> NP ;
    -- ExtAdvNP : NP -> Adv -> NP ;

    -- unclear status: n_units_of_NP : Card -> CN -> NP -> NP ;
}
