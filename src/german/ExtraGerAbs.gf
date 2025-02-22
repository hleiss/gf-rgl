--# -path=.:../abstract:../common:../api:../prelude:

abstract ExtraGerAbs =
  Extra [
    Tense, Temp, Pol, Conj, Prep, Num, Det, Predet, Quant, CN, NP, AP,
      VV, VP, S, Adv, IAdv, IComp, IQuant, Cl, ClSlash, RCl, Utt,
{-      GenNP, EmptyRelSlash,
      VPS, ListVPS, BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS,
      VPI, ListVPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
      ICompAP, IAdvAdv, CompIQuant, PrepCN,
      PastPartAP, PastPartAgentAP, PassVPSlash, PassAgentVPSlash,
      RNP, RNPList, Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP, Conj_RNP,
      ReflRNP, ReflPron, ReflPoss, PredetRNP, AdvRNP, ReflA2RNP, PossPronRNP,
      FocAdV,
-}      Foc, FocObj, FocAdv, FocAP, FocNeg, FocVP, FocVV, UseFoc
  ] ** {
  flags coding=utf8;
  
  cat
    FClause ; -- formal clause
  fun
    PPzuAdv   : CN -> Adv ;  -- zum Lied, zur Flasche
    TImpfSubj : Tense ;      -- ich möchte...   --# notpresent

    moegen_VV : VV ;         -- ich mag/möchte singen

    -- sentential complements with correlates

    ComplCorVS : VS -> S -> VP ; -- glaube daran|es, dass S
    CorVS : VS -> VP ;           -- glaube daran

    -- infinitival complements with correlates

    ComplCorVV : VV -> VP -> VP ; -- versuche es, ein Lied zu singen
    CorVV: VV -> VP ;             -- versuche es|das
    -- replace
    EsVV : VV -> VP -> VP ; -- ich genieße es zu schlafen

    -- So far, there is no category VSA
    -- ComplCorVSA : VSA -> S -> AP -> VP ; -- finde es schön, dass S
    -- CorVSA : VSA -> AP -> VP ;           -- finde es|das schön
        EsV2A : V2A -> AP -> S -> VP ; -- ich finde es schön, dass ...

  	VPass : V -> FClause ;   -- (es) wird getanzt
  	AdvFor : Adv -> FClause -> FClause ; -- es wird heute gelacht - addition of adverbs
  	FtoCl : FClause -> Cl ;  -- embedding FClause within the RGL, to allow generation of S, Utt, etc.

    Pass3V3 : V3 -> VPSlash ; -- wir bekommen den Beweis erklärt

    -- further constructions using RNP of abstract/Extend.gf: (see ReflGer.gf)

    -- Quaternary verbs (too expensive, examples in gf-rgl/tests/german/TestLexiconGerAbs)
  cat
    V4 ;

    -- Construct sentences in conjunctive mood
  cat
    SConj ;
  fun
    UseConjCl : Temp -> Pol -> Cl -> SConj ; -- clause as sentence in conjunctive

    -- Constructions for sentential complementations of nouns (intended as
    -- substitute for modification rule SentCN : CN -> SC -> CN)

  cat
    NS ; NQ ; NV ;
  fun
    -- SentN2 : N2 -> SC -> CN ;

    UseNS : NS -> CN ;
    ComplNS : NS -> S -> CN ;                -- complement by dass-S
    ComplConjNS : NS -> SConj -> CN ;        -- complement by S in conjunctive

    ComplNQ : NQ -> QS -> CN ;
    ComplNV : NV -> VP -> CN ;

    -- Adjectives with sentential, interrogative or infinitival subject:
    -- cat AS ; AQ ; AV to replace SentAP : AP -> SC -> AP by SentAP : AS -> S -> AP etc.

    PositSentA2 : A2 -> SC -> AP ;  -- limited SentAP, e.g. begierig darauf, etwas zu tun

    -- Construction and usage of adverbials with correlate
  fun
    dort_wo_Subj : Subj ; -- more expls in tests/german/TestLexiconGerAbs

    -- Rule to leave an adverbial correlate when right-extracting the adverbial clause
    -- AdvCorVP : VP -> Adv -> VP ; -- overlaps with inplace AdvVP, if adv.hasCor = False

  -- tests (towards adverbs with correlates)

    RelAdv : Adv -> QS -> Adv ;  -- Plan: dort + wo wächst der Pfeffer => dort, wo der Pfeffer wächst
    -- besser analog zu QuestIAdv : IAdv -> Cl -> QCl
    -- RelAdv : Adv -> Cl -> RCl ;
}
