--# -path=.:../abstract:../common:../api:../prelude:../english

concrete ReflEng of ReflAbs =
  GrammarEng,
  -- CatEng,NounEng,AdjectiveEng,StructuralEng,VerbEng-[SlashV2VNP],SentenceEng,
  ExtendEng[NP,Conj,Predet,Num,CN,VP,Cl,Tense, -- on which the following depend:
            RNP,RNPList,Base_nr_RNP, Base_rn_RNP, Base_rr_RNP,ConjRNP,
            ReflPron,ReflPoss,PredetRNP],
            -- omitted ReflRNP = ComplRSlash, AdvRNP, AdvRVP, AdvRAP, ReflA2RNP
  LexiconEng,ReflLexiconEng
  ** open ResEng, Prelude, (P = ParadigmsEng)
  in {

  -- Part extracted from ExtendEng (except for those omitted above)

  -- lincat
  --   RNP     = {s : Agr => Str} ;
  --   RNPList = {s1,s2 : Agr => Str} ;

  -- lin
  --   ReflRNP vps rnp = insertObjPre (\\a => vps.c2 ++ rnp.s ! a) vps ;
  --   ReflPron = {s = reflPron} ;
  --   ReflPoss num cn = {s = \\a => possPron ! a ++ num.s ! True ! Nom ++ cn.s ! num.n ! Nom} ;
  --   PredetRNP predet rnp = {s = \\a => predet.s ++ rnp.s ! a} ;

  --   AdvRNP np prep rnp = {s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a} ;
  --   AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! a) vp ;
  --   AdvRAP ap prep rnp = {s = \\a => ap.s ! a ++ prep.s ++ rnp.s ! a ; isPre = False} ;

  --   ReflA2RNP a rnp = {
  --     s = \\ag => a.s ! AAdj Posit Nom ++ a.c2 ++ rnp.s ! ag ;
  --     isPre = False
  --     } ;

  --   PossPronRNP pron num cn rnp =
  --     DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = \\_ => rnp.s ! pron.a; a = pron.a})) ;

  -- HL 7/2023: To define reflexive predicates:

  lincat
    -- Ger.RAP = {s : Agr => AForm => Str ; isPre : Bool ; c: Agr => Str * Str ; ext : Str} ; 
    -- Eng.AP = {s : Agr => Str ; isPre : Bool} ;
    RAP = AP ;  -- proud of oneself ; bigger than one's brother
    RAdv = {s : Agr => Str } ; -- in one's (own) house
    -- Eng.CN = {s : Number => Case => Str ; g : Gender} ;
    RCN  = {s : Agr => Number => Case => Str } ;
    -- Eng.NP = {s : NPCase => Str ; a : Agr} ;
    -- Eng.RNP = {s : Agr => Str} ;
    -- RNP = {s : Agr => NPCase => Str} ;  -- isPron?
    
  lin
    -- RAP:
    ComplRA2 a rnp = lin AP {
      s = \\agr => a.s ! AAdj Posit Nom ++ a.c2 ++ rnp.s ! agr ;
      isPre = False
      } ;
    ComparRA a rnp = lin AP {
      s = \\agr => getCompar Nom a ++ "than" ++ rnp.s ! agr ;
      isPre = False
      } ;
    CAdvRAP ad ap rnp = lin AP {
      s = \\agr => ad.s ! Pos ++ ap.s ! agr ++ ad.p ++ rnp.s ! agr ;
      isPre = False
      } ;

    -- RAdv: generalizes Extend. AdvRNP : np prep rnp to AdvNP np radv
    PrepRNP p rnp = {s = \\agr => preOrPost p.isPre p.s (rnp.s ! agr) } ;

    -- RCN:
    ComplRN2 n2 rnp = {s = \\agr,n,c => n2.s ! n ! Nom ++ n2.c2 ++ rnp.s ! agr ; g = n2.g} ;
    PossRNP cn rnp = {
      s = \\agr,n,c =>
              cn.s ! n ! c ++ "of" ++ rnp.s ! agr ;
              -- case rnp.isPron of {
              -- -- True => cn.s ! a ! n ! c ++ "von" ++ reflPron ! agr ! Dat; 
              -- -- True => reflPossPron agr n cn.g c ++ cn.s ! Strong ! n ! c ;
              --    True => let eigen = adjForms "eigen" "eigen"
              --            in eigen ! (AMod (gennum cn.g n) c) ++ cn.s ! a ! n ! c ;
              --    False => cn.s ! a ! n ! c ++ rnp.s ! agr ! Gen } ;
      g = cn.g } ;

    -- RNP
    DetRCN det rcn = {
      -- s = \\c => det.s ++ cn.s ! det.n ! npcase2case c ;  DetCN.s
      s = \\agr => det.s ++ rcn.s ! agr ! det.n ! Nom ; 
      a = agrgP3 det.n rcn.g
     } ;


    -- Reflexive complements of copula verbs
    CompRAP rap = rap ;
    CompRAdv radv = {s = \\agr => radv.s!agr} ;
    CompRCN cn = {s = \\agr => case (fromAgr agr).n of {
      Sg => artIndef ++ cn.s ! agr ! Sg ! Nom ;
      Pl => cn.s ! agr ! Pl ! Nom
      }
     } ;
    CompRNP rnp = {s = \\agr => rnp.s ! agr } ;

    -- VPSlash and VP with reflexive nominal complement 
    
    SlashR2V3 v rnp =
      insertObjc (\\agr => v.c2 ++ rnp.s ! agr)
                    (predVc v ** {c2 = v.c3 ; gapInMiddle = False}) ;
    SlashR3V3 v np =
      insertObjc (\\agr => v.c3 ++ np.s ! agr) (predVc v) ; ----

    ComplRSlash vps rnp = insertObjPre (\\a => vps.c2 ++ rnp.s ! a) vps ; -- = ExtendEng.ReflRNP vp np ;

    ComplSlashRAdv vps np radv = -- ad hoc, wrong adv position
       insertAdV (radv.s ! np.a) (insertObj (\\a => vps.c2 ++ np.s ! NPAcc) vps) ;

    PredVPRAdv np vp radv =      -- ad hoc
      mkClause (np.s ! npNom) np.a (AdvVP vp (ss (radv.s ! np.a))) ;

    -- GenericCl vp = mkClause "one" AgP3SgGen vp ; -- to get "oneself", "one's" 
                                                    -- corrected in IdiomEng

  linref
    RAP = \ap -> ap.s ! AgP3SgGen ;
    RAdv = \adv -> adv.s ! AgP3SgGen ;
    RCN  = \rcn -> rcn.s ! AgP3SgGen ! Sg ! Nom ;
--    RNP = \rnp -> rnp.s ! AgP3SgGen ;  -- conflicted with ExtendEng, moved there
--    VP = \vp -> "to"++ infVP VVAux vp False Simul CPos (AgP3SgGen Sg) ;
--    VP = \s -> predV {s = \\_ => s; p = ""; isRefl = False} ;
}
