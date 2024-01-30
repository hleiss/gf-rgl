concrete PhraseGer of Phrase = CatGer ** open Prelude, ResGer in {

  flags optimize=all_subs ;

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = {s = s.s ! Main} ;
    UttQS qs = {s = qs.s ! QDir} ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg False} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Pl False} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg True} ;

    UttIP ip = {s = ip.s ! Nom} ; --- Acc also
    UttIAdv iadv = iadv ;
    UttNP np = {s = np.s ! False ! Nom ++ bigNP np} ;
    UttVP vp = {s = useInfVP True vp} ;  -- without zu
    UttAdv adv = {s = adv.s ++ adv.rc ++ adv.cp} ;
    UttCN n = {s = n.s ! Strong ! Sg ! Nom ++ n.adv ++ n.ext ++ n.rc ! Sg} ;
    UttCard n = {s = n.s ! AMod (GSg Neutr) Nom} ;
    UttAP ap = {s = ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ++ ap.ext ++ ap.s2 ! Nom} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = ss (conj.s2) ;

    NoVoc = {s = []} ;
    VocNP np = {s = "," ++ np.s ! False ! Nom ++ bigNP np} ;

}
