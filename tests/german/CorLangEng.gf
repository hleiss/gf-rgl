--# -path=.:../../src/abstract:../../src/common:../../src/prelude:../../src/api:../../src/english:../../src/german

concrete CorLangEng of CorLang = 
  LangEng - [part_Prep, possess_Prep, that_Subj, SentCN, SentAP],
  CorrelatesEng,
  TestLexiconEng
  ** {} ;
