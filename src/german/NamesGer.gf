concrete NamesGer of Names = CatGer ** open ResGer, Prelude, (P=ParadigmsGer) in {

lin GivenName gn = {
      s = \\_,c => gn.s ! c ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;
      rc, ext = []
      } ;

lin MaleSurname sn = {
      s = \\_,c => sn.s ! Male ! c ;
      a = agrgP3 Masc Sg ;
      w = WLight ;
      rc, ext = []
      } ;

lin FemaleSurname sn = {
      s = \\_,c => sn.s ! Female ! c ;
      a = agrgP3 Fem Sg ;
      w = WLight ;
      rc, ext = []
      } ;

lin PlSurname sn = {
      s = \\_,c => sn.s ! Male ! c ;
      a = agrgP3 Masc Pl ;
      w = WLight ;
      rc, ext = []
      } ;

lin FullName gn sn = {
      s = \\_,c => gn.s ! Nom ++ sn.s ! gn.g ! c ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;
      rc, ext = []
      } ;

-- UseLN : LN -> NP ;
lin UseLN ln = let d = ln.hasDefArt in {
      s = \\b,c => case d of {
                     True  => case b of {
                                True  => [] ; -- defart dropped
                                False => artDef ! (gennum ln.g ln.n) ! c
                              } ++
                              ln.s ! (adjfCase Weak c) ! c ;
                     False => ln.s ! Strong ! c
                   } ;
      a = agrgP3 ln.g ln.n ;
      w = case d of {True => WDefArt ; _ => WLight} ; -- enable contraction with prep, e.g. zur Schweiz
      rc, ext = []
      } ;

-- PlainLN : LN -> NP ;
lin PlainLN ln = {
      s = \\_,c => ln.s ! Strong ! c ;
      a = agrgP3 ln.g ln.n ;
      w = WLight ;
      rc, ext = []
      } ;

-- InLN : LN -> Adv ;
lin InLN ln = let d = ln.hasDefArt in {
      s = appPrepNP P.inDat_Prep {
             s = \\b,c => case d of {
                            True  => case b of {
                                       True  => [] ; -- defart dropped
                                       False => artDef ! (gennum ln.g ln.n) ! c
                                     } ++
                                     ln.s ! (adjfCase Weak c) ! c ;
                            False => ln.s ! Strong ! c
                          } ;
             a = agrgP3 ln.g ln.n ;
             w = case d of {True => WDefArt ; _ => WLight} ; -- e.g. in dem Iran => im Iran
             rc, ext = []
        } ;
      cp,cor = [] ; hasCor,t = False
      } ;

-- AdjLN : AP -> LN -> LN ;
lin AdjLN ap ln = ln ** {
      s = \\a,c => case ap.isPre of {
        True => (ap.c.p1 ++ ap.c.p2 ++ ap.s ! agrAdj a (gennum ln.g ln.n) c ++
                   ln.s ! a ! c ++ ap.s2 ! c ++ ap.ext) ;
        False => ln.s ! a ! c ++
          embedInCommas (ap.c.p1 ++ ap.c.p2 ++ ap.s ! APred ++ ap.s2 ! c ++ ap.ext)} ;
      } ;

}
