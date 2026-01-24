--# -path=.:../abstract:../common:../prelude: 

concrete TestLexiconGer of TestLexiconGerAbs = 
  LexiconGer, CorrelatesGer[NS,NV,NQ,CorAdv,VSA,AS,AV,AQ] **
  open (R=ResGer), (P=Prelude), ParadigmsGer, (Irreg=IrregGer)
in {

flags
  coding = utf8 ;

  -- Verbs

oper
  reflV2 : V -> Case -> Prep -> V2 ; -- reflexive, with case and prep-object
  reflV2 v c p = prepV2 (reflV v c) p ;

  reflV3 : V -> Case -> Prep -> Prep -> V3 ; -- reflexive, with case, and prep-objects
  reflV3 v c p q = mkV3 (reflV v c) p q ;

  -- control verbs
  dirV2V : V -> V2V = \v -> mkV2V v ; -- object control verb;
                                      -- subjV2V v2v = subject-control verb

  -- binary verb with sentential subject (not enforced)
  dassV2 : V -> Prep -> V2 = mkV2 ;

  mkVSA : V -> CorrelatesGer.VSA = \v -> v ** {lock_VSA = <>} ;

  -- -- quaternary verbs (cf. DictionaryGer) 
  -- mkV4 : V -> Prep -> Prep -> Prep -> V4 =
  --   \v,p2,p3,p4 -> lin V4 (v ** { c2=p2 ; c3=p3 ; c4=p4 }) ;
  -- dirV4 : V -> Prep -> Prep -> V4 = \v,c,d -> mkV4 v accPrep c d ;

  mkAQ : A -> Prep -> AQ = \a,p -> lin AQ (a ** {c2 = p});
  -- mkAS : A -> Prep -> AS = \a,p -> lin AS (a ** {c2 = p}); -- conflict with ParadigmsGer.mkAS
  -- mkAV : A -> Prep -> AQ = \a,p -> lin AV (a ** {c2 = p});

lin
  aendern_rV = reflV (regV "ändern") accusative ;
  anstrengen_rV = reflV (prefixV "an" (regV "strengen")) accusative ;
  geschehen_V = Irreg.geschehen_V ;

  gedenken_gen_V2 = mkV2 (irregV "gedenken" "gedenkt" "gedachte" "gedächte" "gedacht") genPrep ;
  bedienen_gen_rV2 = reflV2 (regV "bedienen") accusative genPrep ;
  stuetzen_auf_rV2 = reflV2 (regV "stützen") accusative (mkPrep "auf" accusative) ;
  ergeben_dat_rV2 = reflV2 (irregV "ergeben" "ergibt" "ergab" "ergäbe" "ergeben") accusative datPrep ;
  merken_rV2 = reflV2 (regV "merken") dative accPrep ;
  erstaunen_sV2 = dassV2 (irregV "erstaunen" "erstaunt" "erstaunte" "erstaunte" "erstaunt") accPrep ;

  fragen_VQ = mkVQ (regV "fragen") nach_Prep ;
  fragen_V2 = mkV2 (regV "fragen") nach_Prep ;

  erklaeren_dat_V3 = mkV3 (irregV "erklären" "erklärt" "erklärte" "erklärte" "erklärt") ;
  anklagen_gen_V3 = dirV3 (prefixV "an" (regV "klagen")) genPrep ;
  erinnern_an_V3 = dirV3 (irregV "erinnern" "erinnert" "erinnerte"
                                 "erinnerte" "erinnert") (mkPrep "an" accusative) ;
  erinnern_an_V2S = mkV2S (irregV "erinnern" "erinnert" "erinnerte"
                                 "erinnerte" "erinnert") accPrep (mkCPrep "an" accusative) ;
  erinnern_an_V2V = mkV2V (irregV "erinnern" "erinnert" "erinnerte"
                                 "erinnerte" "erinnert") accPrep (mkCPrep "an" accusative) ;
  danken_dat_fuer_V3 = mkV3 (regV "danken") datPrep (mkPrep "für" accusative) ;
  write_to_V3 = dirV3 Irreg.schreiben_V ((mkPrep "an" accusative) | datPrep) ;
  debattieren_mit_ueber_V3 = mkV3 (irregV "debattieren" "debattiert" "debattierte"
                                          "debattierte" "debattiert") mit_Prep (mkPrep "über" accusative) ;
  lehren_V3 = dirV3 (regV "lehren") accPrep ;

  abschauen_bei_rV3 = reflV3 (prefixV "ab" (irregV "schauen" "schaut"
                                                   "schaute" "schaute" "geschaut")) dative accPrep bei_Prep ;
  leihen_von_rV3 = reflV3 (irregV "leihen" "leiht" "lieh" "liehe" "geliehen") dative accPrep von_Prep ;

  entschuldigen_bei_fuer_rV3 = reflV3 (irregV "entschuldigen" "entschuldigt" "entschuldigte"
                                              "entschuldigte" "entschuldigt") accusative bei_Prep fuer_Prep ;
  raechen_am_fuer_rV3 = reflV3 (regV "rächen") accusative (mkPrep "an" dative) fuer_Prep ;

  -- verbs with infinitival object (non-modal):
  wagen_VV = mkVV (regV "wagen")  ;
  versuchen_VV = mkVV (irregV "versuchen" "versucht" "versuchte" "versuchte" "versucht") ;
  denken_an_VV = (mkVV (irregV "denken" "denkt" "dachte" "dächte" "gedacht")) ** {c2 = ans_Prep} ;

  -- object control verbs:
  helfen_V2V = mkV2V (irregV "helfen" "hilft" "half" "hälfe" "geholfen") datPrep bei_Prep ;
  warnen_V2V = dirV2V (regV "warnen") ** {c3 = vor_Prep} ;
  lassen_V2V = auxV2V (irregV "lassen" "läßt" "ließ" "ließe" "gelassen") accPrep ;
                                            -- lasse dich (*zu) arbeiten
  -- subject control verb:
  versprechen_dat_V2V = subjV2V (mkV2V (irregV "versprechen" "verspricht"
                                               "versprach" "verspräche" "versprochen") datPrep) ;
  -- aci verb:
  sehen_V2V  = auxV2V (irregV "sehen" "sieht" "sah" "sähe" "gesehen") accPrep ;
  hoeren_V2V = auxV2V (regV "hören") accPrep ;

  erwarten_V2 = mkV2 (irregV "erwarten" "erwartet" "erwartete" "erwarte" "erwartet") ;

  -- verb with sentential object
  erinnern_rVS =
    mkVS (reflV (irregV "erinnern" "erinnert" "erinnerte" "erinnerte" "erinnert") accusative) genPrep ;
  trauen_VS = mkVS (regV "trauen") datPrep ;

  finden_VSA = mkVSA (irregV "finden" "findet" "fand" "fände" "gefunden")  ;

  -- quaternary verb:

  -- kaufen_bei_fuer_V4 = dirV4 (regV "kaufen") bei_Prep fuer_Prep ;
  -- mieten_von_fuer_V4 = dirV4 (regV "mieten") von_Prep fuer_Prep ;

  -- Adjectives

  ander_A = mkA "ander" ;
  froh_A  = mkA "froh" ;

  neugierig_auf_A2 = mkA2 (mk3A "neugierig" "neugieriger" "neugierigste") aufs_Prep ;
  treu_A2 = mkA2 (mk3A "treu" "treuer" "treueste") datPrep ;
  stolz_A2 = mkA2 (mk3A "stolz" "stolzer" "stolzeste") (mkCPrep "auf" accusative) ;
  ausgehend_A2 = mkA2 (mkA "ausgehend") von_Prep ;
  einhergehend_A2 = mkA2 (mkA "einhergehend") mit_Prep ;

  glad_AS = lin AS (mkA2 (mkA "froh") (mkCPrep "über" accusative)) ;
  tired_of_AS = lin AS (mkA2 (mkA "überdrüssig") genPrep) ;

  determined_AV = (mkA "entschlossen") ** {c2 = (mkCPrep "zu" dative)} ;
  eager_AV = (mkA "bestrebt") ** {c2 = accPrep} ;

  uncertain_AQ = mkAQ (mkA "ungewiß" "ungewiss" "ungewisser" "ungewisseste") genPrep ;
  curious_AQ = mkAQ (mkA "neugierig") aufAcc_Prep ;

  true_A =  mkA "wahr" ;
  unknown_A = mkA "unbekannt" ;  

  -- Adverbs

--  anders_Adv  = mkAdv "anders" ;
  nirgends_Adv = mkAdv "nirgends";
  ueberall_Adv = mkAdv "überall";
--  anders_als_CAdv = mkCAdv "anders" "als" ; -- (nicht) anders bewertet als

  dieser_Tage_Adv = mkAdv "dieser Tage";

  -- Conjunctions

  neither7nor_DConj = mkConj "weder" "noch" ;
  notonly_butalso_Conj =mkConj "nicht nur" (P.bindComma ++ "sondern auch");

  -- Subjunctions

--  even_though_Subj = mkSubj "auch wenn" ;

  -- Noun

  alp_N = mkN "Alp" "Alpen" feminine ;
  belief_N = mkN "Glaube" "Glauben" masculine ;
  claim_N = mkN "Behauptung" feminine ;
  hope_N = mkN "Hoffnung" feminine ;
  idea_N = mkN "Idee" "Ideen" feminine ;
  intention_N = mkN "Absicht" "Absichten" feminine ;
  interesse_N = mkN "Interesse" "Interessen" neuter ;
  
  belief_NS = mkN "Glaube" "Glauben" masculine ** {c2 = ans_Prep} ;
  claim_NS = mkN "Behauptung" feminine ** {c2 = accPrep} ;

  hope_NV = mkN "Hoffnung" feminine ** {c2 = aufs_Prep} ;
  intention_NV = mkN "Absicht" "Absichten" feminine ** {c2 = zu_Prep} ;
  interesse_NV = mkN "Interesse" "Interessen" neuter ** {c2 = an_Prep} ;

  question_NQ = mkN "Frage" feminine ** {c2 = nach_Prep} ;
  doubt_NQ = mkN "Zweifel" "Zweifel" masculine ** {c2 = an_Prep} ;
{-
  -- alternative
  belief_N2 = mkN "Glaube" "Glauben" masculine ** {c2 = an_Prep} ;
  interesse_N2 = mkN "Interesse" "Interessen" neuter ** {c2 = an_Prep} ;
  question_N2 = mkN "Frage" feminine ** {c2 = nach_Prep} ;
  hope_N2 = (mkN "Hoffnung" feminine) ** {c2 = aufs_Prep} ;
-}
  -- Proper name

  mary_PN = mkPN "Maria" ;

  -- Location name

  switzerland_LN = defLN (mkLN "Schweiz" "Schweiz" feminine) ;

  -- Determiner

  how8much_IDet = {s = table {R.Fem => R.caselist  "wieviel" "wieviel" "wievieler" "wievieler" ;
                              _ => R.caselist "wieviel" "wieviel" "wievielem" "wievielen"} ;
                   a = R.Strong ;
                   n = R.Sg} ;

  beide_Det = {s,sp = \\_,_ => R.caselist "beide" "beide" "beiden" "beider" ;
               a = R.Weak ; ----
               n = R.Pl ;
               isDef, hasDefArt = P.False} ;

  -- Prepositions

--  fuer_Prep  = mkPrep "für" accusative ;
--  mit_Prep   = mkPrep "mit" dative ;
  wegen_Prep = mkCPrep "wegen" dative ;
  wegen2_Prep = mkCPrep [] genitive "wegen" ;          -- postposition
  entlang_Prep  = mkCPrep "entlang" genitive ;
  entlang2_Prep = mkCPrep accusative "entlang" ;
  ueber3_Prep   = mkCPrep accusative "über" ;
  um_herum_Prep = mkCPrep "um" accusative "herum" ;    -- circumposition
  von_aus_Prep  = mkCPrep "von" dative "aus" ;

  -- Contracted Prepositions (Relativparticle, Duden 610, RAdv), with correlate

lin
  an_Prep = mkCPrep "an" "am" "an der" "am" dative ;
  ans_Prep = mkCPrep "an" "an den" "an die" "ans" accusative ;
  auf_Prep = mkCPrep "auf" dative ;
  aufs_Prep = mkCPrep "auf" "auf den" "auf die" "aufs" accusative ;
  aus_Prep = mkCPrep "aus" dative ;
  -- bei_Prep = mkCPrep "bei" "beim" "bei der" "beim" dative ;
  durch_Prep = mkCPrep "durch" "durch den" "durch die" "durchs" accusative ;
  fuer_Prep = mkCPrep "für" accusative ;
  gegen_Prep = mkCPrep "gegen" accusative ;
  von_her_Prep = mkCPrep "von" dative "her" ;
  zu_hin_Prep = mkCPrep "zu" dative "hin" ;
  hinter_Prep = mkCPrep "hinter" dative ;
  -- in_Prep = mkCPrep "in" "im" "in der" "im" dative ;
  -- ins_Prep = mkCPrep "in" "in den" "in die" "ins" accusative ;
  ins_Prep = {s = prepForms "in" "in den" "in die" "ins" "darein" "worein" ;
              s2 = [] ; c = accusative ; t = R.isPrep} ; -- isContracting
  in_hinein_Prep = mkCPrep "in" accusative "hinein" ;
  mit_Prep = mkCPrep "mit" dative ;
  nach_Prep = mkCPrep "nach" dative ;
  neben_Prep = mkCPrep "neben" dative ;
  ueber_Prep = mkCPrep "über" dative ;
  uebers_Prep = mkCPrep "über" accusative ;
  um_Prep = mkCPrep "um" accusative ;
  unter_Prep = mkCPrep "unter" dative ;
  unters_Prep = mkCPrep "unter" accusative ;
  -- von_Prep = mkCPrep "von" "vom" "von der" "vom" dative ; -- ParadigmsGer
  vor_Prep = mkCPrep "vor" dative ;
  -- zu_Prep = mkCPrep "zu" "zum" "zur" "zum" dative ; -- ParadigmsGer
  zwischen_Prep = mkCPrep "zwischen" dative ;
  zwischens_Prep = mkCPrep "zwischen" accusative ;

}
