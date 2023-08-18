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
    duty_N = mkN "Pflicht" "Pflichten" feminine ;
    hero_N = mkN "Held" "Helden" masculine ;
    home_N = mkN "Wohnung" feminine ;
    niece_N = mkN "Nichte" "Nichten" feminine ;
    office_N = mkN "Büro" "Büros"neuter ;
    relation_N2 = mkN2 (mkN "Beziehung" feminine) (mkPrep "zu" dative);
    room_N = mkN "Zimmer" "Zimmer" neuter ;

    im_Prep = mkPrep "in" dative ;
}
