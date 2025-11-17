--# -path=.:../abstract:../common:../api:../prelude

abstract ExtraGerAbs = Extra [
  VPI,ListVPI,BaseVPI,ConsVPI,MkVPI,ComplVPIVV,ConjVPI,ClSlash,RCl,
  VPS,ListVPS,BaseVPS,ConsVPS,ConjVPS,MkVPS,PredVPS,EmptyRelSlash,
  VPSlash, PassVPSlash, PassAgentVPSlash, CompIQuant, PastPartAP, PastPartAgentAP,
  Temp,Tense,Pol,S,NP,VV,VP,Conj,IAdv,IQuant,IComp,ICompAP,IAdvAdv,Adv,AP,
  Cl, Num, CN, Utt, Predet,
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


-- Some constructions intended to replace SC, SentAP, SentNP, ImpersCl, e.a.
-- SentCN : CN -> SC -> CN: question, whether QS ; claim that S ; hope to VP
--  may need subcategories QN, SN, VN of N and complementations Compl : QN -> QS -> CN etc.

    -- sentential subject with correlate

    CorPredSCVP : SC -> VP -> Cl ;  -- it happens often that I sleep
    CorSCVP : VP -> Cl ;            -- it happens rarely (= ImpersCl ?)

    -- sentential object with correlate

    CorComplVS : VS -> S -> VP ; -- glaube daran|es, dass S
    CorVS : VS -> VP ;           -- glaube daran

    Compl2VS : VS -> NP -> VP ;  -- glaube an eure Hilfe (np object to vs)

    -- infinitival object with correlate

    CorComplVV : VV -> VP -> VP ; -- versuche es, ein Lied zu singen (was: EsVV)
    CorVV: VV -> VP ;             -- versuche es|das

    -- interrogative object with correlate
    CorComplVQ : VQ -> QS -> VP ; -- frage danach, wann ihr kommt
    CorVQ: VQ -> VP ;             -- frage es|danach

    -- for verbs with nominal and sentential object with sentential correlate

    CorSlashV2S : V2S -> S -> VPSlash ; -- überzeuge (ihn) davon , dass die Sonne scheint
    CorV2S : V2S -> VPSlash ;           -- überzeuge (ihn) davon

    CorSlashV2V : V2V -> VP -> VPSlash ; -- bitte (dich) darum , nicht zu schlafen
    CorV2V : V2V -> VPSlash ;            -- bitte (dich) darum

    CorSlashV2Q : V2Q -> QS -> VPSlash ; -- frage (ihn) danach , ob die Sonne scheint
    CorV2Q : V2Q -> VPSlash ;            -- frage (ihn) danach

{-  With ComplSlash : VPSlash -> NP -> VP, the following can be derived

    -- sentential object with correlate (for ternary verb)

    CorComplV2S v np s = ComplSlash (CorSlashV2S v s) np -- überzeuge ihn davon , dass ...
    CorV2S v np        = ComplSlash (CorV2S v) np        -- überzeuge ihn davon

    -- infinitival object with correlate (for ternary verb)

    CorComplV2V v np vp = ComplSlash (CorSlashV2V v vp) np -- bitte dich darum , zu arbeiten
    CorV2V v np         = ComplSlash (CorV2V v) np         -- bitte dich darum

    -- interrogative object with correlate (for ternary verb)
    CorComplV2Q v np q  = ComplSlash (CorSlashV2Q v q) np  -- frage ihn danach, wann ihr kommt
    CorV2Q v np         = ComplSlash (CorV2Q v) np         -- frage ihn danach
                                              -- pron.switch: antworte es ihm ?
-}
    -- To use nominal instead of sentential objects
    Compl3V2S : V2S -> NP -> VPSlash ; -- überzeuge (ihn) von der These
    Compl3V2V : V2V -> NP -> VPSlash ; -- bitte (ihn) um einen Rat
    Compl3V2Q : V2Q -> NP -> VPSlash ; -- frage (ihn) nach einer Auskunft

  cat
    VSA ; -- variant of V2A; e.g. finde, dass es schneit, schön
    VVA ; --                 e.g. finde, im See zu schwimmen, schön
  fun
    CorVSA : VSA -> AP -> VP ;           -- finde es|das sehr schön            consider it harmless
    ComplVSA : VSA -> S -> AP -> VP ;    -- finde sehr schön, dass es schneit; consider harmless that S
    CorComplVSA : VSA -> S -> AP -> VP ; -- finde es sehr schön, dass S;       consider it harmless that S
    CorComplVVA : VSA -> VP -> AP -> VP ; -- finde es gut, zu schwimmen

    -- obsolete by CorComplVV resp. CorComplVSA, but keep for backward-compatibility:
	EsVV : VV -> VP -> VP ;        -- ich genieße es zu schlafen
        EsV2A : V2A -> AP -> S -> VP ; -- ich finde es schön, dass ...

  cat
    FClause ; -- formal clause

  fun
  	VPass : V -> FClause ;   -- (es) wird getanzt
  	AdvFor : Adv -> FClause -> FClause ; -- es wird heute gelacht - addition of adverbs
  	FtoCl : FClause -> Cl ;  -- embedding FClause within the RGL, to allow generation of S, Utt, etc.

    -- further constructions using RNP of abstract/Extend.gf: (see ReflGer.gf)

    -- Quaternary verbs (too expensive, expls in gf-rgl/tests/german/TestLexiconGerAbs)
  cat V4 ;
      
    -- Sentences in conjunctive mood
  cat
    SConj ;
  fun
    UseConjCl : Temp -> Pol -> Cl -> SConj ; -- clause as sentence in conjunctive

    -- Noun with sentential complement (to replace SentCN : CN -> SC -> CN)
  cat
    NS ; NQ ; NV ;                    -- can be Noun if sc-complement goes to N.ext
  fun                                 -- Ger: N2, for Frage danach , wo wir leben
    UseNS : NS -> CN ;
    CorNS : NS -> CN ;                -- Glaube daran
    ComplNS : NS -> S -> CN ;         -- claim that we sleep
    ComplConjNS : NS -> SConj -> CN ; -- claim we would sleep
    CorComplNS : NS -> S -> CN ;      -- Glaube daran , dass sie schlafen

    UseNV : NV -> CN ;                -- Hoffnung
    CorNV : NV -> CN ;                -- Hoffnung darauf
    ComplNV : NV -> VP -> CN ;        -- Hoffnung , zu überleben
    Compl2NV : NV -> NP -> CN ;       -- Hoffung auf einen Erfolg
    CorComplNV : NV -> VP -> CN ;     -- Hoffnung darauf , zu überleben

    UseNQ : NQ -> CN ;
    CorNQ : NQ -> CN ;                -- Frage danach
    ComplNQ : NQ -> QS -> CN ;        -- question where we live
    Compl2NQ : NQ -> NP -> CN ;       -- Frage nach einem Grund
    CorComplNQ : NQ -> QS -> CN ;     -- Frage danach , wo wir leben

{-    -- simpler alternative for binary noun with sentential object
  fun
    SentN2 : N2 -> SC -> CN ;
    SentCorN2 : N2 -> SC -> CN ;
    UseN2 : N2 -> CN ;
    CorN2 : N2 -> CN ;

-}
    -- Adjective with sentential complement (to replace SentAP : AP -> SC -> AP).

  fun -- to replace the modification rule SentAP : AP -> SC -> AP
    SentA2 : A2 -> SC -> AP ;     -- begierig , etwas zu tun
    CorSentA2 : A2 -> SC -> AP ;  -- begierig darauf , etwas zu tun
    CorA2 : A2 -> AP ;            -- begierig darauf
{-
  cat
    AS ; AQ ; AV ;
  fun
    ComplAS : AS -> S -> AP ;       -- (we are) glad that they sleep  froh_über:A2=AV
    ComplAV : AV -> VP -> AP ;      -- (we are) happy to be alive
    ComplAQ : AQ -> QS -> AP ;      -- (we are) uncertain whether they arrived
    -- can't add a comparison np to the AV.comp: (she is) more happy than he to live in Europe
    -- ComplAVComp : AV -> NP -> VP -> AP ; -- AV.s!comp ++ np.s ! av.s2 ++ vp.inf
-}
    -- Interrogatve correlate for sentential object
    -- ICorVS : VS -> QVP ;          -- woran glauben
    -- PredIQVP : QVP -> NP -> QCl ; -- woran glauben die Kinder


    -- Some specific constructions in addition to AdjLN : AP -> LN -> LN
    AdjNSg : A -> N -> LN ;       -- (the) Black Sea, (das) Schwarze Meer
    AdjNPl : A -> N -> LN ;       -- (the) Rocky Mountains, (die) Blauen Berge
    CardLN : Card -> N -> LN ;    -- (the) Five Islands, (die) Drei Zinnen, non-loc: Drei Musketiere
    OrdSgLN : Ord -> N -> LN ;    -- (the) Second Cataract, non-loc: Zweite Weltkrieg

    -- Interrogative verb phrase IVP (instead we use QVP for double questions)

    -- further constructions using RNP, declared in abstract/Extend.gf:

    AdvRNP : NP -> Prep -> RNP -> RNP ;   -- a dispute with his wife
    AdvRVP : VP -> Prep -> RNP -> VP ;    -- lectured about her travels
    AdvRAP : AP -> Prep -> RNP -> AP ;    -- adamant in his refusal

    ReflA2RNP : A2 -> RNP -> AP ;         -- indifferent to their surroundings
                                               -- NOTE: generalizes ReflA2

    PossPronRNP : Pron -> Num -> CN -> RNP -> NP ; -- his abandonment of his wife and children

}
