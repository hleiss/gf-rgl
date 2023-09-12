--# -path=.:../abstract:../common:prelude

concrete GrammarGer of Grammar = 
  NounGer,
--  VerbGer,
  VerbGer - [SlashV2VNP], -- HL
  AdjectiveGer,
  AdverbGer,
  NumeralGer,
  SentenceGer,
  QuestionGer,
  RelativeGer,
  ConjunctionGer,
  PhraseGer,
  TextX - [Tense,Temp],
  IdiomGer,
  StructuralGer,
  TenseGer,
  NamesGer
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
