concrete NounGer of Noun = CatGer ** open ResGer, MorphoGer, Prelude, ParadigmsGer in {

  flags optimize=all_subs ;

{- w = WPron: used to enforce pron.acc < pron.dat: ich gebe es ihr nicht
   w = WDefArt: omit definite article after certain prepositions: an dem Haus => am [] Haus
   w = WLight/WHeavy: light object nps come before negation, heavy ones afterwards
   Example: I don't see the man -- ich sehe den Mann nicht vs. 
            I don't see a man   -- ich sehe nicht einen Mann (=> sehe keinen Mann)
   HL 6/2019 (but:) sehe (die|einige) Männer nicht
-}
-- Remark: np.isLight/w makes ResGer.insertObjNP expensive, for ComplSlash, SlashVP

  lin
    DetCN det cn = {
      s = \\c => det.s ! cn.g ! c ++
        (let k = toCase c in cn.s ! adjfCase det.a k ! det.n ! k ++ cn.adv) ;
      a = agrgP3 cn.g det.n ;  
      w = case det.hasDefArt of {
        True => WDefArt ;
        _ => case det.isDef of { True => WLight ; _ => WHeavy } } ;
      rc = cn.rc ! det.n ;   
      ext = cn.ext 
      } ;

    DetNP det = {
      s = \\c => det.sp ! Neutr ! c ; -- more genders in ExtraGer 
      a = agrP3 det.n ;
      w = case det.isDef of { -- no pronoun switch: ich gebe ihr das  vs. ich gebe es ihr
        True => WLight ;      -- not WDefArt: an+dem = "an dem", not "am"
        _ => WHeavy } ; 
      rc, ext = []
      } ;

    UsePN pn = {
      s = \\c => pn.s ! (toCase c) ;
      a = agrgP3 pn.g Sg ;
      w = WLight ;
      rc, ext = []
      } ;

    UsePron pron = {
      s = \\c => pron.s ! NPCase (toCase c) ;
      a = pron.a ;
      w = WPron ;
      rc, ext = []
      } ;

    PredetNP pred np = 
      let ag = case pred.a of {PAg n => agrP3 n ; _ => np.a} in np ** {
        s = \\c0 => 
          let c = case pred.c.k of {NoCase => c0 ; PredCase k => k} in
          pred.s ! numberAgr ag ! genderAgr np.a ! c0 ++ pred.c.p ++ np.s ! c ; 
        a = ag ;
        w = WHeavy 
        } ;

    PPartNP np v2 = np ** {
      s = \\c => np.s ! c ++ embedInCommas (v2.s ! VPastPart APred) ; --- invar part
      w = WHeavy 
      } ;
    -- "eine erfolgreiche Frau, geliebt von vielen,"  but only with v2 not possible in German?
    -- HL: PPartNP np vps|vp: "der Autor, heute vergessen" , "der Mond, gerade aufgegangen,"
	
    AdvNP np adv = np ** {
      s = \\c => np.s ! c ++ adv.s ;
      w = WHeavy 
      } ;

    ExtAdvNP np adv = np ** {
      s = \\c => np.s ! c ++ embedInCommas adv.s ;
      w = WHeavy 
      } ;

    DetQuantOrd quant num ord = 
      let 
        n = num.n ;
        a = quant.a
      in {
        s  = \\g,c => quant.s  ! num.isNum ! n ! g ! c ++ (let k = toCase c in
                        num.s!g!k ++ ord.s ! agrAdj g (adjfCase a k) n k) ;
        sp = \\g,c => quant.sp ! num.isNum ! n ! g ! c ++ (let k = toCase c in
                        num.s!g!k ++ ord.s ! agrAdj g (adjfCase quant.aPl k) n k) ;
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        hasDefArt = quant.isDefArt 
        } ;

    DetQuant quant num = 
      let 
        n = num.n ;
        a = quant.a
      in {
        s  = \\g,c => quant.s  ! num.isNum ! n ! g ! c ++ num.s!g!(toCase c) ;
        sp = \\g,c => quant.sp ! num.isNum ! n ! g ! c ++ num.s!g!(toCase c) ; 
                      -- HL: der+er,den+en ; der drei,den drei+en
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        hasDefArt = quant.isDefArt 
        } ;


    PossPron p = {
      s  = \\_,n,g,c => p.s ! NPPoss (gennum g n) (toCase c) ;
      sp = \\_,n,g,c => p.s ! NPPoss (gennum g n) (toCase c) ;
      a = Strong ;
      aPl = Weak ;
      isDefArt = False 
      } ;

    NumCard n = n ** {isNum = True} ;

    NumPl = {s = \\g,c => []; n = Pl ; isNum = False} ; 
    NumSg = {s = \\g,c => []; n = Sg ; isNum = False} ; 

    NumDigits numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;
    OrdDigits numeral = {s = \\af => numeral.s ! NOrd af} ;

    NumNumeral numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;
    OrdNumeral numeral = {s = \\af => numeral.s ! NOrd af} ;

    AdNum adn num = {s = \\g,c => adn.s ++ num.s!g!c; n = num.n } ;

    OrdSuperl a = {s = a.s ! Superl} ;

    OrdNumeralSuperl n a = {s = \\af => n.s ! NOrd APred ++ Predef.BIND ++ a.s ! Superl ! af} ; -- drittbeste

    DefArt = {
      s = \\_,n,g,c => artDefContr (gennum g n) c ; 
      sp = \\_,n,g,c  => case <n,c> of {
        <Pl,NPC Dat> => "denen" ; -- HL 6/2019
        <Pl,NPC Gen> => "deren" ; -- HL 6/2019  also: derer, die ...
        _ => artDefContr (gennum g n) c } ;  -- von den+en
      isDefArt = True ; -- HL
      a, aPl = Weak
      } ;

    IndefArt = {
      s = table {
        True => \\_,_,c => [] ;
        False => table {
          Sg => \\g,c => "ein" + pronEnding ! GSg g ! (toCase c) ;  
          Pl => \\_,c => []
          }
        } ; 
      sp = table {
        True => \\_,_,c => [] ;
        False => table {
          Sg => \\g,c => (detLikeAdj False Sg "ein").s ! g ! NPC (toCase c) ;
          Pl => \\_,c => caselist "einige" "einige" "einigen" "einiger" ! (toCase c)
          }
        } ;
      isDefArt = False ; -- HL
      a, aPl = Strong 
      } ;

    MassNP cn = {
      s = \\c => cn.s ! Strong ! Sg ! (toCase c) ++ cn.adv ;
      a = agrgP3 cn.g Sg ;
      w = WLight ; -- ? ich trinke Bier nicht vs. ich trinke kein Bier
      rc = cn.rc ! Sg ;
      ext = cn.ext
      } ;

    UseN, UseN2 = \n -> {
      s = \\_ => n.s ;
      g = n.g ;
      rc = \\_ => [] ;
      ext,adv = [] 
      } ;

    ComplN2 f x = {
      s = \\_,n,c => f.s ! n ! c ++ appPrepNP f.c2 x ;
      g = f.g ;
	  rc = \\_ => [] ;
	  ext,adv = [] 
      } ;

    ComplN3 f x = {
      s = \\n,c => f.s ! n ! c ++ appPrepNP f.c2 x ;
      co = f.co ++ appPrepNP f.c2 x ; ---- should not occur at all; the abstract syntax is problematic in giving N2
      uncap = {
        s = \\n,c => f.uncap.s ! n ! c ++ appPrepNP f.c2 x ;
        co = f.uncap.co ++ appPrepNP f.c2 x ; ---- should not occur at all; the abstract syntax is problematic in giving N2
       } ;
      g = f.g ; 
      c2 = f.c3 ;
      } ;

    Use2N3 f = f ;

    Use3N3 f = f ** {
      c2 = f.c3;
      } ;

    AdjCN ap cn = 
      let 
        g = cn.g 
      in cn ** {
        s = \\a,n,c => 
               preOrPost ap.isPre
                 (ap.c.p1 ++ ap.c.p2 ++ ap.s ! agrAdj g a n c ++ ap.ext)
                 (cn.s ! a ! n ! c) ;
        g = g
        } ;

 
    RelCN cn rs = cn ** {rc = \\n => (cn.rc ! n ++ embedInCommas (rs.s ! RGenNum (gennum cn.g n)))} ;
    ---- another layer of embedInCommas needed if there is a non-empty rc
    
    RelNP np rs = np ** {
      rc = (np.rc ++ embedInCommas (rs.s ! RGenNum (gennum (genderAgr np.a) (numberAgr np.a)))) ;
      w = case isPron np of { True => WLight ; _ => np.w } 
      } ;

    SentCN cn s = cn ** {ext = cn.ext ++ embedInCommas s.s} ;

    AdvCN cn a = cn ** {adv = cn.adv ++ a.s} ;

    ApposCN  cn np = let g = cn.g in cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ np.s ! NPC c ++ bigNP np } ;

    PossNP cn np = cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ appPrepNP von_Prep np } ;
}
