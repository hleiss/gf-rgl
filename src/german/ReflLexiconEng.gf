concrete ReflLexiconEng of ReflLexicon = CatEng **
  open (R=ResEng), ParadigmsEng, Prelude in {

  -- for testing
  lin
    intelligent_A = compoundA (mkA "intelligent") ;
    own_A = compoundA (mkA "own");
    proud_A2 = mkA2 (mkA "proud" "prouder") (mkPrep "of") ;

    dispute_N2 = mkN2 (mkN "dispute" "disputes") (mkPrep "with") ;
    duty_N = mkN "duty" "duties" ;
    best_N = mkN "best" ;
    family_N = mkN "family" "families" ;
    hero_N = mkN "hero" "heroes" ;
    home_N = mkN "home" "homes" ;
    niece_N = mkN "niece" "nieces" ;
    office_N = mkN "office" "offices" ;
    parent_N = mkN "parent" "parents" ;
    relation_N2 = mkN2 (mkN "relation" "relations") (mkPrep "to") ;
    room_N = mkN "room" ;
    son_N = mkN "son" "sons" ;
    youth_N = mkN "youth" "youths";

    im_Prep = mkPrep "in" ;

    advise_V2V = defaultV2V (regV "advise") ;     -- typ=VVInf
    promise_V2V = defaultV2V (regV "promise") ;   -- typ=VVInf
    surrender_V2 = mkV2 (regV "surrender") (mkPrep "to") ;

oper
  defaultV2V : V -> V2V = \v -> lin V2V (dirV2 v ** {c3=[] ; typ = R.VVInf }) ;

}
