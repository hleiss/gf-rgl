--# -path=.:../abstract:../common:../prelude:../api:../../tests/german:

concrete CorLangGer of CorLang =
  LangGer - [part_Prep, possess_Prep, that_Subj, SentCN, SentAP],
  CorrelatesGer,
  TestLexiconGer
  ** {} ;
