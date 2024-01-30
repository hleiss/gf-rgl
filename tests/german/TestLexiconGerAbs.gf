--# -path=.:../abstract:../common:../prelude: -- partially extracted from DictVerbsGerAbs
abstract TestLexiconGerAbs = Lexicon ** {

  -- Verbs

fun
  aendern_rV : V ;
  anstrengen_rV : V ;

  gedenken_gen_V2 : V2 ;
  bedienen_gen_rV2 : V2 ;
  stuetzen_auf_rV2 : V2 ;
  ergeben_dat_rV2 : V2 ;
  merken_rV2 : V2 ;

  anklagen_gen_V3 : V3 ;
  erklaeren_dat_V3 : V3 ;
  lehren_V3 : V3 ;
  erinnern_an_V3 : V3 ;
  write_to_V3 : V3 ;
  danken_dat_fuer_V3 : V3 ;
  debattieren_mit_ueber_V3 : V3 ;

  abschauen_bei_rV3 : V3 ;
  leihen_von_rV3 : V3 ;

  entschuldigen_bei_fuer_rV3 : V3 ;
  raechen_am_fuer_rV3 : V3 ;

  wagen_VV : VV ;
  versuchen_VV : VV ;

  helfen_V2V : V2V ;          -- -aux(zu-inf), object control
  warnen_V2V : V2V ;          -- -aux,         object control
  versprechen_dat_V2V : V2V ; -- -aux,         subject control
  lassen_V2V : V2V ;          -- +aux(inf),    object control
  -- aci verb:                -- +aux(inf),    object control
  sehen_V2V : V2V ;
  hoeren_V2V : V2V ; 

  erwarten_V2 : V2 ;
  
cat 
  V4 ;
fun
  kaufen_bei_fuer_V4 : V4 ;
  mieten_von_fuer_V4 : V4 ;

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

  -- Prepositions

  fuer_Prep : Prep ;
  mit_Prep : Prep ;
  wegen_Prep : Prep ;
  wegen2_Prep : Prep ;
  entlang_Prep : Prep ;   -- pre  + genitive
  entlang2_Prep : Prep ;  -- post + accusative
  um_herum_Prep : Prep ;  -- cirumposition
  von_aus_Prep : Prep ;
  
  -- Noun

  idea_N : N ;
  intention_N : N ;

  -- Proper name

  mary_PN : PN ;

  -- Determiner

  how8much_IDet : IDet ;
}
