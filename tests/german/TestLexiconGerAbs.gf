--# -path=.:../../src/german: -- for ExtraGerAbs; verbs partially extracted from DictVerbsGerAbs
abstract TestLexiconGerAbs = Lexicon, CorrelatesGerAbs[NS,NV,NQ,VSA] ** {

  -- Verbs

fun
  aendern_rV : V ;
  anstrengen_rV : V ;
  geschehen_V : V ;

  gedenken_gen_V2 : V2 ;
  bedienen_gen_rV2 : V2 ;
  stuetzen_auf_rV2 : V2 ;
  ergeben_dat_rV2 : V2 ;
  merken_rV2 : V2 ;

  erstaunen_sV2 : V2 ; -- sentential subject (not enforced)

  fragen_VQ : VQ ;
  fragen_V2 : V2 ;

  anklagen_gen_V3 : V3 ;
  erklaeren_dat_V3 : V3 ;
  lehren_V3 : V3 ;
  erinnern_an_V3 : V3 ;
  erinnern_an_V2S : V2S ;
  erinnern_an_V2V : V2V ;
  write_to_V3 : V3 ;
  danken_dat_fuer_V3 : V3 ;
  debattieren_mit_ueber_V3 : V3 ;

  abschauen_bei_rV3 : V3 ;
  leihen_von_rV3 : V3 ;

  entschuldigen_bei_fuer_rV3 : V3 ;
  raechen_am_fuer_rV3 : V3 ;

  wagen_VV : VV ;
  versuchen_VV : VV ;
  denken_an_VV : VV ;
  helfen_V2V : V2V ;          -- -aux(zu-inf), object control
  warnen_V2V : V2V ;          -- -aux,         object control
  versprechen_dat_V2V : V2V ; -- -aux,         subject control
  lassen_V2V : V2V ;          -- +aux(inf),    object control
  -- aci verb:                -- +aux(inf),    object control
  sehen_V2V : V2V ;
  hoeren_V2V : V2V ; 

  erwarten_V2 : V2 ;

  finden_VSA : VSA ;          -- finde AP, dass S

  -- kaufen_bei_fuer_V4 : V4 ;
  -- mieten_von_fuer_V4 : V4 ;

  -- Adjectives

  ander_A : A ;
  froh_A : A ;

  neugierig_auf_A2 : A2 ;
  treu_A2 : A2 ;  -- Ger: dative complement
  stolz_A2 : A2 ;
  ausgehend_A2 : A2 ;
  einhergehend_A2 : A2 ;

  -- Adverbs

--  anders_Adv : Adv ;
  nirgends_Adv : Adv ;
  ueberall_Adv : Adv ;
  anders_als_CAdv : CAdv ;

  dieser_Tage_Adv : Adv ;

  -- Conjunctions

  neither7nor_DConj : Conj ;
  notonly_butalso_Conj : Conj ;

  -- Subjunctions

  even_though_Subj : Subj ;

  -- Prepositions

  fuer_Prep : Prep ;
  mit_Prep : Prep ;
  wegen_Prep : Prep ;
  wegen2_Prep : Prep ;
  entlang_Prep : Prep ;   -- pre  + genitive
  entlang2_Prep : Prep ;  -- post + accusative
  ueber3_Prep : Prep ;
  um_herum_Prep : Prep ;  -- cirumposition
  von_aus_Prep : Prep ;

  -- Noun

  idea_N : N ;
  intention_N : N ;
  alp_N : N ;

  belief_NS : NS ;
  claim_NS : NS ;

  hope_NV : NV ;
  intention_NV : NV ;
  interesse_NV : NV ;
  question_NQ : NQ ;
  doubt_NQ : NQ ;
{-
  -- alternative:
  hope_N2 : N2 ;
  belief_N2 : N2 ;
  interesse_N2 : N2 ;
  question_N2 : N2 ;
-}
 -- Proper name

  mary_PN : PN ;

  -- Location name

  switzerland_LN : LN ;

 -- Determiner

  how8much_IDet : IDet ;
  beide_Det : Det ;

fun
  -- Contracting prepositions (including relativ particle (RAdv) Duden 610)

  an_Prep : Prep ;
  ans_Prep : Prep ;
  auf_Prep : Prep ;
  aufs_Prep : Prep ;
  aus_Prep : Prep ;
  -- bei_Prep : Prep ; -- in ParadigmsGer
  durch_Prep : Prep ;
  fuer_Prep : Prep ;
  gegen_Prep : Prep ;
  von_her_Prep : Prep ;
  zu_hin_Prep : Prep ;
  hinter_Prep : Prep ;
  -- in_Prep : Prep ; -- in StructuralGer
  ins_Prep : Prep ;
  in_hinein_Prep : Prep ;
  mit_Prep : Prep ;
  nach_Prep : Prep ;
  neben_Prep : Prep ;
  ueber_Prep : Prep ;
  uebers_Prep : Prep ;
  um_Prep : Prep ;
  unter_Prep : Prep ;
  unters_Prep : Prep ;
  -- von_Prep : Prep ; -- in ParadigmsGer
  vor_Prep : Prep ;
  zu_Prep : Prep ;
  zwischen_Prep : Prep ;
  zwischens_Prep : Prep ;

}
