--# -path=.:../abstract:../common:../api:../prelude

-- HL 7/2023: Extend Lang by generic reflexive predicates e.g. "to love oneself and one's family"

concrete ReflGer of Refl =
  GrammarGer - [UttVP], -- redefine UttVP below
  -- ExtendGer[NP,Conj,Predet,Num,CN,VP,Cl,Tense, -- on which the following depend:
  --           RNP,RNPList,Base_nr_RNP, Base_rn_RNP,Base_rr_RNP,ConjRNP,
  --           Cons_nr_RNP, Cons_rr_RNP,
  --           ReflPron,ReflPoss,PredetRNP],
  -- Q: Why does this import not work? Because of types of NP,...,Tense?
  LexiconGer,
  ReflLexiconGer
  ** open ResGer, Prelude, Coordination in {

  -- All rnp:RNP (except ReflPron.a = AgSgP3Gen?) can and should be given an agreement
  -- rnp.a, which is needed in combination with object-control verbs:
  --   to advise (one's brother|sister):RNP to help (him|her)self:ReflPron!rnp.a

  -- Part copied rather than imported from ExtendGer:

  lincat
    RNP = {s : Agr => Case => Str ; rc,ext : Str ; isPron : Bool} ; -- missing: a:Agr
    RNPList = {s1,s2 : Agr => Case => Str} ;

  linref
    RNP = \rnp -> rnp.s ! AgSgP3Gen ! Acc ++ rnp.ext ++ rnp.rc ;

  lin
    -- ReflRNP vps rnp = insertObjReflNP rnp vps ;  -- now ComplRSlash below

    ReflPron = {            -- in Nom as personal pronoun
      s = ResGer.reflPron ; rc,ext = [] ; isPron = True } ;

    ReflPoss num cn =
      {s = \\a,c => let adjf = case num.n of {Sg => Strong ; Pl => Weak} -- Duden 477, HL 5/2022
         in possPron a num.n cn.g c ++ num.s ! cn.g ! c -- HL 5/2022: meine wenigstens 3 cn,
            ++ cn.s ! adjfCase adjf c ! num.n ! c       --       not: wenigstens 3 meine cn
            ++ cn.adv ;
       ext = cn.ext ; rc = cn.rc ! num.n ;
       isPron = False} ;

    -- We might define ReflPoss by the stronger reflPossPron below, using "eigen(er)"
    -- to distinguish possessive pronoun from reflexive possessive pronoun:
    --   du kennst meine Fehler vs. ich kenne meine eigenen Fehler
    --   er|sie|es kennt seine|ihre Fehler  vs. er|sie|es kennt seine|ihre|seine eigenen Fehler

    PredetRNP pred rnp = rnp ** {                        -- HL 5/2022
      s = \\a,c => let n : Number = case pred.a of {PAg n => n ; _ => numberAgr a} ;
                       g : Gender = genderAgr a ;
                       d = case pred.c.k of {NoCase => c ; PredCase k => k} ;
        in case rnp.isPron of {
          True => pred.s ! Pl ! Masc ! c ++ "von" ++ rnp.s ! a ! Dat ;
          _ => pred.s ! n ! genderAgr a ! c ++ pred.c.p ++ rnp.s ! a ! d} ;
      ext = rnp.ext ; rc = rnp.rc ;
      isPron = False} ;
      -- ok: alle von uns; die meisten von uns ; wrong: *nur von uns =/= nur wir

    ConjRNP conj rnps = conjunctDistrTable2 Agr Case conj rnps
      ** {isPron = False ; ext,rc = []} ;

    Base_rr_RNP x y = twoTable2 Agr Case x y ;
    Base_nr_RNP x y = twoTable2 Agr Case {s = \\_,c => x.s ! False ! c ++ x.ext ++ x.rc} y ;
    Base_rn_RNP x y = twoTable2 Agr Case x {s = \\_,c => y.s ! False ! c ++ y.ext ++ y.rc} ;

    Cons_rr_RNP x xs = consrTable2 Agr Case comma x xs ;
    Cons_nr_RNP x xs = consrTable2 Agr Case comma {s = \\_,c => x.s ! False ! c ++ x.ext ++ x.rc} xs ;

  --- Part with new material for reflexive predicates:

  lincat
    RAP = {s : AForm => Str ; isPre : Bool ;
           c: Agr => Str * Str ;     -- (stolz auf) sich (selbst)
           ext : Agr => Str} ;       -- (größer als) sein (eigener) Bruder

    RAdv = {s : Agr => Str} ;  -- in one's (own) house
    RCN  = {
      s : Agr => Adjf => Number => Case => Str ;
      adv : Str ;          -- Haus    [adv auf dem Hügel]          -- ? Agr => ...
      rc : Number => Str ; -- Frage , [rc die ich gestellt habe]   -- ? Agr => ...
      ext : Agr => Str ;   -- älterer Mann [als Johann]
      g : Gender
      } ;
    -- would need a category RComp with type {... ; ext : Agr => Str}, see CompRAP

  lin
    -- Constructions of reflexive AP, Adv, CN and NP (Todo: check position of rnp.rc+ext)
    
    ComplRA2 a rnp = 
      let obj : Agr => Str * Str = case a.c2.isPrep of {
			isCase => \\agr => <appPrep a.c2 (rnp.s!agr), []> ;
			_      => \\agr => <[], appPrep a.c2 (rnp.s!agr)> }
      in { s = a.s ! Posit ;
           isPre = True ;
           c = obj ;
           ext = \\agr => rnp.ext ++ rnp.rc
      } ;
    ComparRA a rnp = {
      s = \\af => a.s ! Compar ! af ;
      isPre = True ;
      c = \\agr => <[],[]> ;
      ext = \\agr => conjThan ++ rnp.s ! agr ! Nom ++ rnp.rc ++ rnp.ext
      } ;
    CAdvRAP ad ap rnp = {
      s = \\af => ad.s ++ ap.c.p1 ++ ap.s ! af ++ ap.c.p2 ;
      isPre = False ;
      c = \\agr => <[],[]> ;
      ext = \\agr => ap.ext ++ ad.p ++ rnp.s ! agr ! Nom ++ rnp.ext ++ rnp.rc
      } ;
      
    PrepRNP p rnp = {s = \\agr => appPrep p (rnp.s ! agr)} ;
    ComparRAdvAdj cadv a np = {
      s = \\agr => cadv.s ++ a.s ! Posit ! APred ++ cadv.p ++ np.s ! agr ! Nom
      } ;

    ComplRN2 n2 rnp = {
      s = \\agr,_,n,c => n2.s ! n ! c ++ appPrep n2.c2 (rnp.s ! agr) ;
      g = n2.g ;
      rc = \\_ => [] ;
      ext = \\agr => [] ;
      adv = [] } ;
    ComplRN3 n3 rnp np = {
      s = \\agr,_,n,c => n3.s ! n ! c ++ appPrep n3.c2 (rnp.s ! agr)
                                      ++ appPrep n3.c3 (np.s ! agr) ;
      g = n3.g ;
      rc = \\_ => [] ;
      ext = \\agr => [] ;
      adv = [] } ;
    PossRNP cn rnp = {
      s = \\agr,a,n,c => case rnp.isPron of {
              -- True => cn.s ! a ! n ! c ++ "von" ++ reflPron ! agr ! Dat; 
              -- True => reflPossPron agr n cn.g c ++ cn.s ! Strong ! n ! c ;
                 True => let eigen = adjForms "eigen" "eigen"
                         in eigen ! (AMod (gennum cn.g n) c) ++ cn.s ! a ! n ! c ;
                 False => cn.s ! a ! n ! c ++ rnp.s ! agr ! Gen }
          ++ rnp.ext ++ rnp.rc ;
      g = cn.g ; rc = cn.rc ; ext = \\agr => cn.ext ; adv = cn.adv } ;
    AdjRCN rap cn =
      let g = cn.g in {
        s = \\agr,a,n,c => preOrPost rap.isPre
                 ((rap.c ! agr).p1 ++ (rap.c ! agr).p2 ++ rap.s ! agrAdj g a n c)
                 (cn.s ! a ! n ! c) ;
        ext = \\agr => cn.ext ++ rap.ext ! agr ;
        rc = cn.rc ;
        adv = cn.adv ;
        g = cn.g
        } ;


    -- ExtendGer: ReflPoss num cn : RNP
    DetRCN det cn = {
      s = \\agr,c => det.s ! False ! cn.g ! c ++
                     cn.s ! agr ! (adjfCase det.a c) ! det.n ! c ++ cn.adv
                     ++ cn.ext ! agr ;
      isPron = False ;
      rc = cn.rc ! det.n ;
      ext = []
      } ;

    -- Reflexive complements of copula verbs
    -- (skips RCom.ext:Agr => Str: (weil man) s:älter (ist) ext:als seine Kinder)
    CompRAP rap = {
      s = \\agr => (rap.c!agr).p1 ++ rap.s ! APred ++ (rap.c!agr).p2 ;
      ext = rap.ext ! agrP3 Sg  -- bug: would need \\agr => rap.ext ! agr
      } ;
    CompRAdv radv = {s = \\agr => radv.s!agr ; ext = []} ;
    CompRCN rcn = {
        s = \\a => case numberAgr a of {
            Sg => "ein" + pronEnding ! GSg rcn.g ! Nom
                   ++ rcn.s ! a ! Strong ! Sg ! Nom ++ rcn.adv ++ rcn.rc ! Sg ;
            Pl => rcn.s ! a ! Strong ! Pl ! Nom ++ rcn.adv ++ rcn.rc ! Pl
            } ++ rcn.ext ! a ;  -- älter als sein Bruder hier (sein)
        ext = []
      } ;
    CompRNP rnp = {s = \\a => rnp.s ! a ! Nom ++ rnp.rc ; ext = rnp.ext} ;

    -- VPSlash and VP with reflexive nominal complement
    
    SlashR2V3 v rnp =
      insertObjRNP (lin RNP rnp) v.c2 (predVc v) ** {c2 = v.c3 ; objCtrl = True} ;
    SlashR3V3 v rnp =
      insertObjRNP (lin RNP rnp) v.c3 (predVc v) ** {c2 = v.c2 ; objCtrl = True} ;

    -- The next is wrong for object-control verbs in vps, if we have no rnp.a:Agr:
    -- For vps = to beg sb to wash (his=sb's car), we would need
    -- ComplRSlash vps (my wife) => to beg (my wife) to wash (her! car)
    ComplRSlash vps rnp =  -- = ReflRNP vps rnp    (except for vps.objCtrl=true)
      let vp = case vps.objCtrl of {
         True => objAgr {a=agrP3 Sg} vps -- should be: objAgr {a=rnp.a} vps
                 ** {c2 = vps.c2 ; objCtrl = True } ;
         _  => vps }
      in insertObjRNP (lin RNP rnp) vps.c2 vp ;

    ComplRVA v rap = insertRAdj (rap.s ! APred) rap.c rap.ext (predV v) ;

    -- VP and Cl with reflexive adverb

    -- expensive: + ComplSlashRAdv 388800 (374400,560)
    ComplSlashRAdv vps np radv =   -- radv refers to np per agreement
      let vp = case vps.objCtrl of { True => objAgr np vps ; _  => vps }
               ** { c2 = vps.c2 ; objCtrl = vps.objCtrl } ;
      in insertAdv (radv.s ! np.a) (insertObjNP np vps.c2 vp) ;

    PredVPRAdv np vp radv =
      let rvp  = insertAdv (radv.s ! np.a) vp ; -- = AdvVP (radv.s ! np.a) vp
          subj = mkSubject np rvp.c1
      in mkClause subj.s subj.a rvp ;           -- = PredVP np (AdvVP (...) vp)

    -- IdiomGer.GenericCl vp = mkClause "man" (agrP3 Sg) vp ;
    -- Avoid a pronoun that would not be a correct object in German:
    -- one_Pron = mkPronPers "man" "einen" "einem" "seiner" "sein" Masc Sg P3
    --            ** { a = AgSgP3Gen } ; -- special agreement for mkClause str agr vp
    -- AgSgP3Gen is needed for Eng (oneself,one's), could be (AgSgP3 Masc) for Ger?

  linref
    RAP = \ap -> (ap.c! AgSgP3Gen).p1 ++ ap.s ! APred ++ (ap.c!AgSgP3Gen).p2 ++ ap.ext ! AgSgP3Gen ;
    RAdv = \adv -> adv.s ! AgSgP3Gen ;
    RCN  = \rcn -> rcn.s ! AgSgP3Gen ! Weak ! Sg ! Nom ++ rcn.adv
                   ++ rcn.ext ! AgSgP3Gen ++ rcn.rc ! Pl ;
    -- VP,VPSlash in CatGer: changed via using AgSgP3Gen in ResGer.infVP

  -- to parse, use infinitive with "zu" (Eng: to+inf), differing from PhraseGer.UttVP:
  lin
    UttVP vp = { s = useInfVP False vp } ; -- infinitive with zu;
    UttVPSlash vp = {s = useInfVP False vp ++ vp.c2.s ! GPl} ;
    UttRAP rap =
      let a:Agr = AgSgP3Gen
      in {s = (rap.c ! a).p1 ++ rap.s ! APred ++ (rap.c ! a).p2 ++ rap.ext ! a} ;
    UttRAdv radv = {s = radv.s ! AgSgP3Gen} ;
    UttRCN rcn =
      let a:Agr = AgSgP3Gen
      in {s = rcn.s ! a ! Weak ! Sg ! Acc ++ rcn.adv ++ rcn.ext ! a ++ rcn.rc ! Pl };
    UttRNP rnp =
      {s = rnp.s ! AgSgP3Gen ! Acc ++ rnp.ext ++ rnp.rc} ;

  oper
    insertObjReflNP : RNP -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,vp -> insertObjRNP rnp vp.c2 vp ;

    insertObjRNP : RNP -> Preposition -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,prep,vp ->                                           -- generalize ResGer.insertObjRefl
      let c = prep.c ;
          obj : Agr => Str = \\a => prep.s ! GPl ++ rnp.s ! a ! c ++ rnp.ext ++ rnp.rc
      in vp ** {
        nn = \\a =>
          let vpnn = vp.nn ! a in
          case <prep.isPrep, rnp.isPron, c> of { -- consider non-pron rnp as light, add to vpnn.p2
            <isCase,True,Acc> => <obj ! a ++ vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4> ; -- pronoun switch:
            <isCase,True,_>   => <vpnn.p1 ++ obj ! a, vpnn.p2, vpnn.p3, vpnn.p4> ; -- accPron < pron
            <isCase,False,_>  => <vpnn.p1, vpnn.p2 ++ obj ! a, vpnn.p3, vpnn.p4> ; -- < non-pron nominal
            <_,     _,    _>  => <vpnn.p1, vpnn.p2, vpnn.p3 ++ obj ! a, vpnn.p4> } --   or prepositional
      } ;

  -- rather ad-hoc: Ger.AP needs reorganization generally
  insertRAdj : Str -> (Agr => Str * Str) -> (Agr => Str) -> ResGer.VP -> ResGer.VP =
    \adj,c,ext,vp -> vp ** {
    nn = \\agr => 
      let vpnn = vp.nn ! agr in <vpnn.p1, vpnn.p2 ++ (c!agr).p1, -- seiner Frau treu
                                 vpnn.p3 ++ (c!agr).p2,          -- neugierig auf ihr Buch
                                 vpnn.p4> ;
    adj = vp.adj ++ adj;
    ext = vp.ext ++ ext!AgSgP3Gen} ;   -- default; the misuse of ext for comparison doesn't work here

}
