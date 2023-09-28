--# -path=.:../common:../abstract:../prelude

concrete ReflLexiconGer of ReflLexicon =
  CatGer ** open ParadigmsGer, (Mo = MorphoGer), Prelude in {

  -- for testing
  lin
    intelligent_A = mk3A "intelligent" "intelligenter" "intelligenteste" ;
    own_A = mk3A "eigen" "eigener" "eigenste" ;
    -- proud_A2 = mkA2 "stolz" (mkPrep "auf" "auf den" "auf die" "aufs" accusative) ;
    proud_A2 = mkA2 (mkA "stolz") (mkPrep "auf" accusative) ;

    best_N = mkN "Bestes" "Besten" neuter ;  -- ein|sein Bestes, missing: das Beste
    dispute_N2 = mkN2 (mkN "Streit" "Streite" masculine) (mkPrep "mit" dative) ;
    duty_N = mkN "Pflicht" "Pflichten" feminine ;
    family_N = mkN "Familie" "Familien" feminine ;
    hero_N = mkN "Held" "Helden" masculine ;
    home_N = mkN "Wohnung" feminine ;
    niece_N = mkN "Nichte" "Nichten" feminine ;
    office_N = mkN "Büro" "Büros" neuter ;
    parent_N = mkN "Elter" "Eltern" masculine ;
    relation_N2 = mkN2 (mkN "Beziehung" feminine) (mkPrep "zu" dative);
    room_N = mkN "Zimmer" "Zimmer" neuter ;
    son_N = mkN "Sohn" "Söhne" masculine ;
    youth_N = mkN "Jugend" feminine ;

    into_Prep = inAcc_Prep ;

    -- object control verb
    advise_V2V = mkV2V (irregV "raten" "rät" "riet" "riete" "geraten") datPrep ;
    -- subject control verb:
    promise_V2V = subjV2V (mkV2V (irregV "versprechen" "verspricht"
                                         "versprach" "verspräche" "versprochen") datPrep) ;
    surrender_V2 = reflV2 (irregV "ergeben" "ergibt" "ergab" "ergäbe" "ergeben") accusative datPrep ;

oper
  reflV2 : V -> Case -> Prep -> V2 ; -- reflexive, with case and prep-object
  reflV2 v c p = prepV2 (reflV v c) p ;

}
