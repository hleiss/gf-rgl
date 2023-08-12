--# -path=.:../common:../abstract:../prelude

concrete ReflLexiconGer of ReflLexicon =
  CatGer ** open ParadigmsGer, (Mo = MorphoGer), Prelude in {

  -- for testing
  lin
    -- proud_A2 = mkA2 "stolz" (mkPrep "auf" "auf den" "auf die" "aufs" accusative) ;
    proud_A2 = mkA2 (mkA "stolz") (mkPrep "auf" accusative) ;
    best_N = mkN "Bestes" "Besten" neuter ;  -- ein|sein Bestes, missing: das Beste
    duty_N = mkN "Pflicht" "Pflichten" feminine ;
    room_N = mkN "Zimmer" "Zimmer" neuter ;
}
