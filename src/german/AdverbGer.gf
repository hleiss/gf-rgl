concrete AdverbGer of Adverb = CatGer ** open ResGer, Prelude in {

  lin
    PositAdvAdj a = {s = a.s ! Posit ! APred ; cp,rc = []} ;

    ComparAdvAdj cadv a np = {
      s = cadv.s ++ a.s ! cadv.deg ! APred ;
      cp = cadv.p ++ np.s ! False ! Nom ++ bigNP np ;
      rc = []
      } ;
    ComparAdvAdjS cadv a s = {
      s = cadv.s ++ a.s ! Posit ! APred ;
      cp = cadv.p ++ s.s ! Sub ;
      rc = []
      } ;

    PrepNP prep np = {s = appPrepNP prep np ; cp,rc = []} ;

    AdAdv ada adv = adv ** {s = ada.s ++ adv.s} ;

    PositAdAAdj a = {s = a.s ! Posit ! APred ; cp,rc = []} ;

    SubjS subj s = {s = subj.s ++ s.s ! Sub ; cp,rc = []} ;

    AdnCAdv cadv = {s = cadv.s ++ conjThan} ; ---- HL: ebenso als ?

}

---b    AdvSC s = s ;
