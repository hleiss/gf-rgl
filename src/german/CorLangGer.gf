--# -path=.:../abstract:../common:../prelude:../api:../../tests/german:

concrete CorLangGer of CorLangGerAbs = 
  LangGer - [that_Subj],
  CorrelatesGer,
  TestLexiconGer
  ** {} ;
