concrete ReflLexiconEng of ReflLexicon = CatEng **
  open ParadigmsEng, Prelude in {

  -- for testing
  lin
    intelligent_A = compoundA (mkA "intelligent") ;
    own_A = compoundA (mkA "own");
    proud_A2 = mkA2 (mkA "proud" "prouder") (mkPrep "of") ;

    duty_N = mkN "duty" "duties" ;
    best_N = mkN "best" ;
    hero_N = mkN "hero" "heroes" ;
    home_N = mkN "home" "homes" ;
    niece_N = mkN "niece" "nieces" ;
    office_N = mkN "office" "offices" ;
    relation_N2 = mkN2 (mkN "relation" "relations") (mkPrep "to") ;
    room_N = mkN "room" ;
    son_N = mkN "son" "sons" ;

    im_Prep = mkPrep "in" ;
}
