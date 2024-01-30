concrete AdverbGer of Adverb = CatGer ** open ResGer, Prelude in {

  lin
    PositAdvAdj a = {s = a.s ! Posit ! APred ; cp,rc = []} ;

    ComparAdvAdj cadv a np = let adv : Str * Str = cadv.s ! False in {
      s = adv.p1 ++ a.s ! cadv.deg ! APred ;
      cp = adv.p2 ++ np.s ! False ! Nom ++ bigNP np ;
      rc = []
      } ;
    ComparAdvAdjS cadv a s = let adv : Str * Str = cadv.s ! False in {
      s = adv.p1 ++ a.s ! cadv.deg ! APred ;
      cp = adv.p2 ++ s.s ! Sub ;
      rc = []
      } ;

    PrepNP prep np = {s = appPrepNP prep np ; cp,rc = []} ;

    AdAdv ada adv = adv ** {s = ada.s ++ adv.s} ;

    PositAdAAdj a = {s = a.s ! Posit ! APred ; cp,rc = []} ;

    SubjS subj s = {s = subj.s ++ s.s ! Sub ; cp,rc = []} ;

    AdnCAdv cadv = let adv : Str * Str = cadv.s ! True in {s = adv.p1 ++ adv.p2} ;

}

---b    AdvSC s = s ;
