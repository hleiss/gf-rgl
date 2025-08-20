concrete AdverbGer of Adverb = CatGer ** open ResGer, Prelude in {

  lin PositAdvAdj a = {s = a.s ! Posit ! APred ; cp,cor = [] ; hasCor,isClause = False} ;

    ComparAdvAdj cadv a np = let adv : Str * Str = cadv.s ! False in {
      s = adv.p1 ++ a.s ! cadv.deg ! APred ;
      cp = adv.p2 ++ np.s ! False ! Nom ++ bigNP np ;
      cor = [] ; hasCor,isClause = False
      } ;
    ComparAdvAdjS cadv a s = let adv : Str * Str = cadv.s ! False in {
      s = adv.p1 ++ a.s ! cadv.deg ! APred ;
      cp = adv.p2 ++ s.s ! Sub ;
      cor = [] ; hasCor,isClause = False
      } ;

    PrepNP prep np = {s = appPrep prep np ; cp,cor = [] ; hasCor,isClause = False} ;

    AdAdv ada adv = adv ** {s = ada.s ++ adv.s} ;

    PositAdAAdj a = {s = a.s ! Posit ! APred ; cp,cor = [] ; hasCor,isClause = False} ;

    SubjS subj s = {s = subj.s ++ s.s ! Sub ; cp = [] ;
                    cor = subj.cor ; hasCor = subj.hasCor ; isClause = True} ;

    AdnCAdv cadv = let adv : Str * Str = cadv.s ! True in {s = adv.p1 ++ adv.p2} ;

}

---b    AdvSC s = s ;
