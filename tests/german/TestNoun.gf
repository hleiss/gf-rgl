abstract TestNoun = 
  Lexicon, -- !
  Noun, 
--  Verb, 
  Adjective,
  Adverb,
  Numeral,
--  Sentence,
--  Question,
--  Relative,
  Conjunction,
  Phrase,
--  TextX - [Tense,Temp],
  Idiom,
  Structural,
  Tense
  ** { 
  flags 
    startcat=Phr ;
  fun
    ans_Prep, aufs_Prep, ins_Prep, ums_Prep : Prep ;
    am_Prep, beim_Prep, zum_Prep, vom_Prep : Prep ;
    wegen_Prep, um_Willen_Prep : Prep ;
    an_vorbei_Prep, um_herum_Prep : Prep ; -- isGlued circumpositions
}
