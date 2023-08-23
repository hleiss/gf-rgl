--# -path=.:../abstract:../common:../api:../prelude

-- HL 7/2023: Extend Lang (minus Coordination, Markup,..) by reflexive predicates

concrete ReflGer of ReflAbs =
  GrammarGer, -- VerbGer omits SlashV2VNP
  -- -- ExtraGer-[AdvRAP,AdvRNP,ReflA2RNP,ReflRNP],
  -- ExtendGer[NP,Conj,Predet,Num,CN,VP,Cl,Tense, -- on which the following depend:
  --           RNP,RNPList,Base_nr_RNP, Base_rn_RNP, Base_rr_RNP,ConjRNP,
  --           ReflPron,ReflPoss,PredetRNP],
  LexiconGer,ReflLexiconGer
  ** open ResGer, Prelude in {

  --- Copied from ExtendGer, but why can't these be imported from ExtendGer[...] ??? ---
  -- because RNP uses Case, NP uses PCase ??

  lincat
    RNP = {s : Agr => Case => Str ; rc,ext : Str ; isPron : Bool} ;
    RNPList = {s1,s2 : Agr => Case => Str} ;

  linref
    RNP = \rnp -> rnp.s ! AgSgP3Gen ! Nom ++ rnp.ext ++ rnp.rc ;

  lin
    ReflRNP vps rnp =
      insertObjReflNP rnp vps ;

    ReflPron = {            -- personal pronoun, with "sich" in P3 Sg
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
                       d = case pred.c.k of {NoCase => c ; PredCase k => (prepC k).c} ;
        in case rnp.isPron of {
          True => pred.s ! Pl ! Masc ! (NPC c) ++ "von" ++ rnp.s ! a ! Dat ;
          _ => pred.s ! n ! genderAgr a ! (NPC c) ++ pred.c.p ++ rnp.s ! a ! d} ;
      ext = rnp.ext ; rc = rnp.rc ;
      isPron = False} ;
      -- ok: alle von uns; die meisten von uns ; wrong: *nur von uns =/= nur wir

  --- new material for reflecxive predicates ---

  lincat
    RAP = {s : Agr => AForm => Str ; isPre : Bool ;
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
    -- in ExtraGer: RNP = {s : Agr => Case => Str ; rc,ext : Str ; isPron : Bool} ;
    -- fehlt: a:Agr, mit ReflPron.a = AgSgP3Gen, und sonst abhängig von det? (one's car).a = Ag Fem Sg P3

  lin
    -- Construcions of reflexive AP, Adv, CN and NP (Todo: check position of rnp.rc+ext)
    
    ComplRA2 a rnp = 
      let obj : Agr => Str * Str = case a.c2.isPrep of {
			False => \\agr => <appPrepC a.c2 (rnp.s!agr), []> ;
			True  => \\agr => <[], appPrepC a.c2 (rnp.s!agr)> } 
      in { s = \\agr => a.s ! Posit ;
           isPre = True ;
           c = obj ;
           ext = \\agr => rnp.ext ++ rnp.rc
      } ;
    ComparRA a rnp = {
      s = \\agr,af => a.s ! Compar ! af ;
      isPre = True ;
      c = \\agr => <[],[]> ;
      ext = \\agr => conjThan ++ rnp.s ! agr ! Nom ++ rnp.rc ++ rnp.ext
      } ;
    CAdvRAP ad ap rnp = {
      s = \\agr,af => ad.s ++ ap.c.p1 ++ ap.s ! af ++ ap.c.p2 ;
      isPre = False ;
      c = \\agr => <[],[]> ;
      ext = \\agr => ap.ext ++ ad.p ++ rnp.s ! agr ! Nom ++ rnp.ext ++ rnp.rc
      } ;
      
    PrepRNP p rnp = {s = \\agr => appPrepC p (rnp.s ! agr)} ;
    ComparRAdvAdj cadv a np = {
      s = \\agr => cadv.s ++ a.s ! Posit ! APred ++ cadv.p ++ np.s ! agr ! Nom
      } ;

    ComplRN2 n2 rnp = {
      s = \\agr,_,n,c => n2.s ! n ! c ++ appPrepC n2.c2 (rnp.s ! agr) ;
      g = n2.g ;
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
                 ((rap.c ! agr).p1 ++ (rap.c ! agr).p2 ++ rap.s ! agr ! agrAdj g a n c)
                 (cn.s ! a ! n ! c) ;
        ext = \\agr => cn.ext ++ rap.ext ! agr ;
        rc = cn.rc ;
        adv = cn.adv ;
        g = cn.g
        } ;


    -- ExtendGer: ReflPoss num cn : RNP
    DetRCN det cn = {
      s = \\agr,c => det.s ! cn.g ! (NPC c) ++
                     cn.s ! agr ! (adjfCase det.a c) ! det.n ! c ++ cn.adv
                     ++ cn.ext ! agr ;
      isPron = False ;
      rc = cn.rc ! det.n ;
      ext = []
      } ;

    -- Reflexive complements of copula verbs
    -- (skips RCom.ext:Agr => Str: (weil man) s:älter (ist) ext:als seine Kinder)
    CompRAP rap = {
      s = \\agr => (rap.c!agr).p1 ++ rap.s ! agr ! APred ++ (rap.c!agr).p2
                   ++ "als" ++ rap.ext ! agr ;
      ext = []
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

    -- VPSlash and VP with reflexive nominal complement  (Todo: check objCtrl)
    
    SlashR2V3 v rnp = insertObjRNP rnp v.c2 (predVc v) ** {c2 = v.c3 ; objCtrl = True} ; 
    SlashR3V3 v rnp = insertObjRNP rnp v.c3 (predVc v) ** {c2 = v.c2 ; objCtrl = True} ; 

    ComplRSlash vps rnp =  -- ? = ReflRNP vps rnp = insertObjReflNP rnp vps ?
      let vp = case vps.objCtrl of { True => objAgr {a=agrP3 Sg} vps ; _  => vps } -- ad hoc
               ** { c2 = vps.c2 ; objCtrl = vps.objCtrl } 
      in insertObjRNP (rnp ** {lock_RNP=<>}) vps.c2 vp ;
    -- todo: ComplRVA : VA -> RAP -> VP ; -- es blauer als sein Haus malen

    -- VP and Cl with reflexive adverb
    -- expensive: + ComplSlashRAdv 388800 (374400,560)
    ComplSlashRAdv vps np radv =   -- radv refers to np per agreement
      let vp = case vps.objCtrl of { True => objAgr np vps ; _  => vps }
               ** { c2 = vps.c2 ; objCtrl = vps.objCtrl } ;
      in insertAdv (radv.s ! np.a) (insertObjNP np vps.c2 vp) ;

    PredVPRAdv np vp radv =
      let rvp  = insertAdv (radv.s ! np.a) vp ; -- = AdvVP (radv.s ! np.a) vp
          subj = mkSubj np rvp.c1
      in mkClause subj.p1 subj.p2 rvp ;         -- = PredVP np (AdvVP (...) vp)

    -- GenericCl vp = mkClause "man" (agrP3 Sg) vp ;
    -- avoid a pronoun that would not be a correct object in German:
    -- one_Pron = mkPronPers "man" "einen" "einem" "seiner" "sein" Masc Sg P3
    --            ** { a = AgSgP3Gen } ; -- special agreement for mkClause str agr vp
    
  linref
    RAP = \ap -> (ap.c! AgSgP3Gen).p1 ++ ap.s ! AgSgP3Gen ! APred ++ (ap.c!AgSgP3Gen).p2 ++ ap.ext ! AgSgP3Gen ;
    RAdv = \adv -> adv.s ! AgSgP3Gen ;
    RCN  = \rcn -> rcn.s ! AgSgP3Gen ! Weak ! Sg ! Nom ++ rcn.adv
                   ++ rcn.ext ! AgSgP3Gen ++ rcn.rc ! Pl ;
    -- VP,VPSlash in CatGer: changed via using AgSgP3GFen in ResGer.infVP
    -- Comp: added to CatGer, to avoid conflict

  oper
    insertObjReflNP : RNP -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,vp -> insertObjRNP rnp vp.c2 vp ;

    insertObjRNP : RNP -> Preposition -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,prep,vp ->                                           -- generalize ResGer.insertObjRefl
      let -- prep = vp.c2 ;
          c = case prep.c of { NPC cc => cc ; _ => Acc } ; -- put rnp.ext ++ rnp.rc to vp.ext ?
          obj : Agr => Str = \\a => prep.s ++ rnp.s ! a ! c ++ rnp.ext ++ rnp.rc
      in vp ** {
        nn = \\a =>
          let vpnn = vp.nn ! a in
          case <prep.isPrep, rnp.isPron, c> of {           -- consider non-pron rnp as light, add to vpnn.p2
            <False,True,Acc> => <obj ! a ++ vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4> ; -- pronoun switch:
            <False,True,_>   => <vpnn.p1 ++ obj ! a, vpnn.p2, vpnn.p3, vpnn.p4> ; -- accPron < pron
            <False,False,_>  => <vpnn.p1, vpnn.p2 ++ obj ! a, vpnn.p3, vpnn.p4> ; -- < non-pron nominal
            <True,_,_>       => <vpnn.p1, vpnn.p2, vpnn.p3 ++ obj ! a, vpnn.p4> } --   or prepositional
      } ;


{- examples (depending on linrefs): ReflPredicates>

p -cat=RAP "verheiratet mit sich"
ComplRA2 married_A2 ReflPron

p -cat=RAP "jünger als es"
ComparRA young_A ReflPron
p -cat=RAP "dümmer als sein Bruder"
ComparRA stupid_A (ReflPoss NumSg (UseN2 brother_N2))

p -cat=RAP "ebenso dumm wie es"
CAdvRAP as_CAdv (PositA stupid_A) ReflPron

p -cat=RAdv "in seinem Haus"
PrepRNP in_Prep (ReflPoss NumSg (UseN house_N))
p -cat=RAdv "in dem Haus seiner Schwester"
PrepRNP in_Prep (DetRCN (DetQuant DefArt NumSg) (PossRNP (UseN house_N) (ReflPoss NumSg (UseN sister_N))))

ComplRN2 (ComplN3 distance_N3 (UsePron we_Pron)) (ReflPoss NumPl (UseN boss_N))
ComplRN2 father_N2 (ReflPoss NumPl (UseN boss_N))

p -cat=RCN "Mutter von seinen Brüdern"
ComplRN2 mother_N2 (ReflPoss NumPl (UseN2 brother_N2))

p -cat=RCN "Entfernung von seinem Chef"
ComplRN2 (Use2N3 distance_N3) (ReflPoss NumSg (UseN boss_N))
p -cat=RCN "Entfernung zu seinem Chef"
ComplRN2 (Use3N3 distance_N3) (ReflPoss NumSg (UseN boss_N))

p -cat=RCN "Entfernung von uns zu seinem Chef"
ComplRN2 (ComplN3 distance_N3 (UsePron we_Pron)) (ReflPoss NumSg (UseN boss_N))
p -cat=RCN "Entfernung von seinem Chef zu uns"
The sentence is not complete                          ????

l PossRNP (UseN house_N) ReflPron
Haus von sich                                  ??? => (sein) eigene(s) Haus
p -cat=RCN "Haus seiner Kinder"
PossRNP (UseN house_N) (ReflPoss NumPl (UseN child_N))
p -cat=RCN "Haus der Kinder seines Bruders"
PossRNP (UseN house_N) (DetRCN (DetQuant DefArt NumPl) (PossRNP (UseN child_N) (ReflPoss NumSg (UseN2 brother_N2))))

p -cat=RNP "sein Haus"
ReflPoss NumSg (UseN house_N)
p -cat=RNP "das Haus seiner Kinder"
DetRCN (DetQuant DefArt NumSg) (PossRNP (UseN house_N) (ReflPoss NumPl (UseN child_N)))

l (DetRCN (DetQuant DefArt NumSg) (PossRNP (UseN house_N) ReflPron))
das Haus von sich            ==> das eigene Haus
l (DetRCN (DetQuant IndefArt NumSg) (PossRNP (UseN house_N) ReflPron))
ein Haus von sich            ==> ein eigenes Haus

p -cat=VP "jünger als er zu sein"
UseComp (CompAP (ComparA young_A (UsePron he_Pron)))
UseComp (CompRAP (ComparRA young_A ReflPron))
p -cat=VP "in seinem Haus zu sein"
UseComp (CompRAdv (PrepRNP in_Prep (ReflPoss NumSg (UseN house_N))))

p -cat=VP "ein Haus seiner Kinder zu sein"
UseComp (CompRCN (PossRNP (UseN house_N) (ReflPoss NumPl (UseN child_N))))

p -cat=VP "Häuser seiner Kinder zu sein"
UseComp (CompRNP (DetRCN (DetQuant IndefArt NumPl) (PossRNP (UseN house_N) (ReflPoss NumPl (UseN child_N)))))

p -cat=VP "er zu sein"
UseComp (CompNP (UsePron he_Pron))
UseComp (CompRNP ReflPron)
p -cat=VP "sein Chef zu sein"
UseComp (CompRNP (ReflPoss NumSg (UseN boss_N)))

l UseComp (CompRNP (DetRCN (DetQuant IndefArt NumPl) (PossRNP (UseN house_N) ReflPron)))
Häuser von sich zu sein     ===> eigene Häuser zu sein

ReflPredicates> p -cat=VP "seinen Hund seinem Chef zu verkaufen"
ComplRSlash (SlashR2V3 sell_V3 (ReflPoss NumSg (UseN dog_N))) (ReflPoss NumSg (UseN boss_N))
ReflRNP (SlashR2V3 sell_V3 (ReflPoss NumSg (UseN dog_N))) (ReflPoss NumSg (UseN boss_N))

p -cat=VPSlash "seinen Hund zu verkaufen"
SlashR2V3 sell_V3 (ReflPoss NumSg (UseN dog_N))
p -cat=VPSlash "seinem Chef zu verkaufen"
SlashR3V3 sell_V3 (ReflPoss NumSg (UseN boss_N))

p -cat=VP "seinem Chef seinen Hund zu verkaufen" 
ComplRSlash (SlashR3V3 sell_V3 (ReflPoss NumSg (UseN boss_N))) 
            (ReflPoss NumSg (UseN dog_N)) 
ReflRNP (SlashR3V3 sell_V3 (ReflPoss NumSg (UseN boss_N))) 
            (ReflPoss NumSg (UseN dog_N))

p -cat=VP "dir den Käse in seinem Haus zu verkaufen"   *ihrem (!)
ComplSlashRAdv (Slash3V3 sell_V3 (UsePron youSg_Pron)) (DetCN (DetQuant DefArt NumSg) (UseN cheese_N)) (PrepRNP in_Prep (ReflPoss NumSg (UseN house_N)))

p -cat=VP "dir die Mütze in ihrem Haus zu verkaufen"   *seinem (!)
ComplSlashRAdv (Slash3V3 sell_V3 (UsePron youSg_Pron)) (DetCN (DetQuant DefArt NumSg) (UseN cap_N)) (PrepRNP in_Prep (ReflPoss NumSg (UseN house_N)))

Reflexive resolution: RAdv refers to (agrees with) v3.c2 object

ReflPredicates> gr -tr (ComplSlashRAdv (Slash3V3 sell_V3 (UsePron youSg_Pron)) (DetCN (DetQuant DefArt NumSg) (UseN ?)) (PrepRNP in_Prep (ReflPoss NumSg (UseN apartment_N)))) | l
ComplSlashRAdv (Slash3V3 sell_V3 (UsePron youSg_Pron)) (DetCN (DetQuant DefArt NumSg) (UseN cap_N)) (PrepRNP in_Prep (ReflPoss NumSg (UseN apartment_N)))

dir die Mütze in ihrer Wohnung zu verkaufen

ReflPredicates> gr -tr (ComplSlashRAdv (Slash3V3 sell_V3 (UsePron youSg_Pron)) (DetCN (DetQuant DefArt NumSg) (UseN ?)) (PrepRNP in_Prep (ReflPoss NumSg (UseN apartment_N)))) | l
ComplSlashRAdv (Slash3V3 sell_V3 (UsePron youSg_Pron)) (DetCN (DetQuant DefArt NumSg) (UseN stone_N)) (PrepRNP in_Prep (ReflPoss NumSg (UseN apartment_N)))

dir den Stein in seiner Wohnung zu verkaufen

p -cat=Cl "er lebt in seiner Wohnung"
PredVPRAdv (UsePron he_Pron) (UseV live_V) (PrepRNP in_Prep (ReflPoss NumSg (UseN apartment_N)))
p -cat=Cl "er lebt in dem Haus seiner Frau"
PredVPRAdv (UsePron he_Pron) (UseV live_V) (PrepRNP in_Prep (DetRCN (DetQuant DefArt NumSg) (PossRNP (UseN house_N) (ReflPoss NumSg (UseN woman_N)))))

-- Reflexive predicates

p -cat=GVP "eigene Biere zu trinken"
GenericVP (ComplRSlash (SlashV2a drink_V2) (DetRCN (DetQuant IndefArt NumPl) (PossRNP (UseN beer_N) ReflPron)))
GenericVP (ReflRNP (SlashV2a drink_V2) (DetRCN (DetQuant IndefArt NumPl) (PossRNP (UseN beer_N) ReflPron)))

l (ComplRSlash (SlashV2a drink_V2) (DetRCN (DetQuant DefArt NumPl) (PossRNP (UseN beer_N) ReflPron)))
die eigene Biere zu trinken

-- wrong Adjf
Refl> l (ComplRSlash (SlashV2a drink_V2) (DetRCN (DetQuant DefArt NumSg) (PossRNP (UseN beer_N) ReflPron)))
das eigenes Bier zu trinken

-}


}
