--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- use the modified files in gf-rgl/src/german (branches prepDefArt*)

concrete TestNounGer of TestNoun = 
  LexiconGer,
  NounGer, 
    VerbGer, 
  AdjectiveGer,
  AdverbGer,
--  NumeralGer,
--  SentenceGer,
--  QuestionGer,
  RelativeGer,
--  ConjunctionGer,
--  PhraseGer,
--  TextX - [Tense,Temp],
  IdiomGer,
  StructuralGer,
  TenseGer
  ** open ParadigmsGer in {
  flags 
    startcat = Phr ; unlexer = text ; lexer = text ;
  lin
    ans_Prep  = mkPrep "an" <"an den","an die","ans"> accusative ;
    aufs_Prep = mkPrep "auf" <"auf den","auf die","aufs"> accusative ;
    ins_Prep  = inAcc_Prep ;
    ums_Prep  = mkPrep "um" <"um den","um die","ums"> accusative ;
    am_Prep   = anDat_Prep ;
    beim_Prep = mkPrep "bei" <"beim","bei der","beim"> dative ;
    vom_Prep  = von_Prep ;
    zum_Prep  = zu_Prep ;
    wegen_Prep = mkPrep genitive "wegen" ;
    um_Willen_Prep = mkPrep "um" genitive "Willen" ;
    an_vorbei_Prep = mkPrep "an" <"am","an der","am"> dative "vorbei" ;
    um_herum_Prep = mkPrep "um" <"um den","um die","ums"> accusative "herum" ;
} ;
