concrete ConjunctionGer of Conjunction = 
  CatGer ** open ResGer, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin

    ConjS conj ss = conjunctDistrTable Order conj ss ;

    ConjAdv conj ss = {s = conj.s1 ++ ss.s1 ++ conj.s2 ++ ss.s2 ; cp,cor = [] ; hasCor,t = False} ;

    ConjAdV conj ss = {s = conj.s1 ++ ss.s1 ++ conj.s2 ++ ss.s2} ;
    ConjIAdv conj ss = {s = conj.s1 ++ ss.s1 ++ conj.s2 ++ ss.s2} ;

    ConjNP conj ss = heavyNP (
    {s = \\_ => (conjunctDistrTable Case conj ss).s ;
     a = let n : Number = (conjNumber conj.n (numberAgr ss.a)) ;
             p : Person = personAgr ss.a ;
             agr : Agr = case <n,p> of {<Pl,q> => AgPl q ;
                                        <Sg,P3> => AgSgP3 Neutr ;
                                        <Sg,P1> => AgSgP1 ;
                                        <Sg,P2> => AgSgP2 }
         in (conjAgr agr ss.a) }) ;

    ConjAP conj ss = conjunctDistrTable AForm conj ss ** {
      s2 = \\c => [] ; -- comparison np of ap = {s:AForm => Str; s2:Case => Str} HL 1/23
      isPre = ss.isPre ; c = ss.c ; ext = ss.ext} ;

    ConjRS conj ss = conjunctDistrTable RelGenNum conj ss ** {
      c = ss.c
      } ;

    ConjCN conj ss = conjunctDistrTable3 Adjf Number Case conj ss ** {
      rc = \\_ => [] ; ext = [] ; adv = [] ; g = ss.g
      } ;

    ConjDet conj ss = {s = \\b,g,c => conj.s1 ++ ss.s1!g!c ++ conj.s2 ++ ss.s2!g!c ;
                       sp = \\b,g,c => conj.s1 ++ ss.sp1!g!c ++ conj.s2 ++ ss.sp2!g!c ;
                       n = conj.n ;
                       a = Weak ; -- guess
                       isDef,hasDefArt = False} ;

-- These fun's are generated from the list cat's.

    BaseS x y = { -- twoTable Order ;
      s1 = x.s ;
      s2 = table {Inv => y.s ! Main ; o => y.s ! o}
      } ;
    ConsS x xs = { -- consrTable Order comma ;
      s1 = \\o => x.s ! Inv ++ comma ++ xs.s1 ! case o of {Inv => Main ; _ => o} ;
      s2 = xs.s2
      } ;

    BaseAdv x y = lin Adv {
                s1 = bigAdv (lin Adv x) ;
		s2 = bigAdv (lin Adv y) ;
		cp,cor = []
      } ;
    ConsAdv xs x = {
		s1 = xs.s ++ comma ++ x.s1 ;
		s2 = x.s2 ;
		} ;
    BaseAdV = twoSS ;
    ConsAdV = consrSS comma ;
    BaseIAdv = twoSS ;
    ConsIAdv = consrSS comma ;
    BaseNP x y = {
		s1 = \\c => x.s ! False ! c ++ bigNP x ;
		s2 = \\c => y.s ! False ! c ++ bigNP y ;
		a = conjAgr x.a y.a } ;
    ConsNP xs x = {
		s1 = \\c => xs.s ! False ! c ++ bigNP xs ++ comma ++ x.s1 ! c ;
		s2 = x.s2 ;
		a = conjAgr xs.a x.a } ;
    BaseAP x y = lin AP {
		s1 = bigAP (lin AP x) ;
		s2 = bigAP (lin AP y) ;
		isPre = andB x.isPre y.isPre ;
		c = <[],[]> ;
	  	ext = []} ;
   ConsAP xs x = lin AP {
		s1 = \\a => (bigAP (lin AP xs)) ! a ++ comma ++ x.s1 ! a ;
		s2 = x.s2 ;
		isPre = andB x.isPre xs.isPre ;
		c = <[],[]> ;
	  	ext = []} ;
    BaseRS x y = twoTable RelGenNum x y ** {c = y.c} ;
    ConsRS xs x = consrTable RelGenNum comma xs x ** {c = xs.c} ;
    BaseCN x y = {
      s1 = bigCN (lin CN x) ;
      s2 = bigCN (lin CN y) ;
      g  = x.g ; --- gender of first CN, used e.g. in articles
      } ; 
    ConsCN x xs = {
      s1 = \\a,n,c => bigCN (lin CN x) ! a ! n ! c ++ comma ++ xs.s1 ! a ! n ! c ;
      s2 = xs.s2 ;
      g  = x.g ; --- gender of first CN, used e.g. in articles
      } ; 
    BaseDAP x y = {
      s1 = x.s ; sp1 = x.sp ;
      s2 = y.s ; sp2 = y.sp ;
      n = y.n ;
      a = y.a } ;
    ConsDAP x xs = {s1 = \\g,c => x.s!g!c ++ comma ++ xs.s1!g!c ;
                    sp1 = \\g,c => x.sp!g!c ++ comma ++ xs.sp1!g!c ;
                    s2 = xs.s2 ; sp2 = xs.sp2 ;
                    n = xs.n ; a = xs.a} ;
      
  lincat
    [S] = {s1,s2 : Order => Str} ;
    [Adv] = {s1,s2 : Str} ;
    [AdV] = {s1,s2 : Str} ;
    [IAdv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Case => Str ; a : Agr} ;
    [AP] = {s1,s2 : AForm => Str ; isPre : Bool; c : Str * Str ; ext : Str} ;
    [RS] = {s1,s2 : RelGenNum => Str ; c : Case} ;
    [CN] = {s1,s2 : Adjf => Number => Case => Str ; g : Gender} ;
    [DAP] = {s1,s2,sp1,sp2 : Gender => Case => Str ; n : Number ; a : Adjf} ;

  oper
    bigAP : AP -> AForm => Str = \ap ->                                         -- HL 1/23: not always ok:
		\\a => ap.c.p1 ++ ap.s ! a ++ ap.c.p2 ++ ap.ext ++ ap.s2 ! Nom ; -- comparison np in Nom
    bigCN : CN -> Adjf => Number => Case => Str = \cn ->
		\\a,n,c => cn.s ! a ! n ! c ++ cn.adv ++ cn.ext ++ cn.rc ! n ;
		
    bigAdv : CatGer.Adv -> Str = \adv -> adv.cor ++ adv.s ++ adv.cp ;
}
