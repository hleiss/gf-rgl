--# -path=.:../abstract:../common:../api:../prelude:

concrete ExtraGer of ExtraGerAbs = CatGer **
  open ResGer, Coordination, Prelude, IrregGer, (P = ParadigmsGer), (N = NounGer), (S = StructuralGer) in {

  flags coding=utf8 ;

  lin
    TImpfSubj  = {s = [] ; t = Past ; m = MConjunct} ;   --# notpresent

    moegen_VV = auxVV mögen_V ;

    Pass3V3 v = -- HL 7/19
      let bekommen : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen")
      in insertObj (\\_ => (v.s ! VPastPart APred)) (predV bekommen) **
           { c1 = PrepNom ; c2 = v.c2 ; objCtrl = False } ;

      -- quaternary verbs:
  lincat
    V4 = Verb ** {c2,c3,c4 : Preposition} ;

-- SS: implementation of some of the relevant Foc rules from Extra

  lincat 
    Foc = {s : Mood => ResGer.Tense => Anteriority => Polarity => Str} ;
	
  lin 
    FocObj np cl =
      let n = appPrep cl.c2 np in (mkFoc n (lin Cl cl)) ;

    FocAdv adv cl = mkFoc adv.s (lin Cl cl) ;

    FocAP ap np =
      let adj = ap.s ! APred ;
          vp = predV ResGer.sein_V ** {ext = ap.c.p1 ++ ap.c.p2 ++ ap.ext};
               -- potentially not correct analysis for all examples
               -- works for:
               -- "treu ist sie ihm"
               -- "froh ist sie dass er da ist"
               -- "stolz ist sie auf ihn"
          subj = mkSubject np vp.c1 ;
          cl = mkClause subj.s subj.a vp
      in mkFoc adj (lin Cl cl) ;

    UseFoc t p f = {s = t.s ++ p.s ++ f.s ! t.m ! t.t ! t.a ! p.p} ;


-- extra rules to get some of the "es" alternative linearisations

  lincat
    VSA = Verb ;

  lin
    EsV2A v2a ap s = predV v2a ** {
      nn = \\_ => <"es",[],[],[]> ;
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;

    CorVSA vsa ap = predV vsa ** {          -- todo: test and document
      adj = ap.s ! APred} ;
    ComplVSA vsa s ap = predV vsa ** {
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;
    ComplCorVSA vsa s ap = predV vsa ** {   -- EsV2A
      nn = \\_ => <"es",[],[],[]> ;
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;
    ComplCorVVA vsa vp ap =
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
      in
      insertExtrapos vp.ext (
        insertInf inf (predV vsa ** { nn = \\_ => <"es",[],[],[]> ;
                                      adj = ap.s ! APred } -- ++ ap.s2 ...
          )) ;

  -- Sentential complement with correlate

  oper
    mkCor : Preposition -> Str = \p ->
      case p.t of {isContracting => p.s ! CAdvPron ; _ => "es" } ; -- | "das"} ;
  lin
    -- correlate for sentential subject
    PredCorSCVP sc vp = mkClause "es" (agrP3 Sg) (insertExtrapos sc.s vp) ;
    -- CorSCVP vp = ImpersCl vp ;

    -- correlate for sentential object
    ComplCorVS vs s =
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub)
      (predV vs ** {c2 = vs.c2 ; cor = mkCor vs.c2}) ;
    CorVS vs =
      predV vs ** {c2 = vs.c2 ; cor = mkCor vs.c2} ;

  -- correlate for infinitival object
  -- lintype VV now has c2:Preposition and cor:Str;  denke daran, .. zu tun | will es tun
    ComplCorVV vv vp =                       -- generalizes former EsVV
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
          vvp = predV vv ** {c2 = vv.c2 ; cor = mkCor vv.c2}
      in
      insertExtrapos vp.ext (insertInf inf vvp) ;
    CorVV vv =
      predV vv ** {c2 = vv.c2 ; cor = mkCor vv.c2} ;

    ComplCorVQ v q =
      insertExtrapos (comma ++ q.s ! QIndir) (predV v ** {c2 = v.c2 ; cor = mkCor v.c2}) ;
    CorVQ v = predV v ** {c2 = v.c2 ; cor = mkCor v.c2} ;

    -- SlashCorV2S : V2S -> S -> VPSlash ; -- überzeuge (ihn) davon , dass die Sonne scheint
    -- CorV2S : V2S -> VPSlash ;           -- überzeuge (ihn) davon

    SlashCorV2S v s =              -- erinnere (jmdn) daran, dass wir schlafen
      predVc v ** {cor = mkCor v.c3 ; ext = comma ++ conjThat ++ s.s ! Sub} ;
    CorV2S v = predVc v ** {cor = mkCor v.c3} ;

    SlashCorV2Q v q =
      predVc v ** {cor = mkCor v.c3 ; objCtrl = False ; ext = comma ++ q.s ! QIndir} ;
    CorV2Q v = predVc v ** {cor = mkCor v.c3} ;

    SlashCorV2V v vp =             -- bitte (jmdn) darum , zu schlafen
      let
        vps = predVGen v.isAux v ; -- e.g. verspricht|bittet.isAux=False | läßt.isAux=True
        inf = mkInf v.isAux Simul Pos vp
      in
      insertExtrapos vp.ext (
        insertInf inf vps) ** {cor = mkCor v.c3 ; c2 = v.c2 ; objCtrl = v.objCtrl} ;
    CorV2V v =                     -- rate (jmdm) dazu
      predVGen v.isAux v ** {cor = mkCor v.c3 ; c2 = v.c2 ; objCtrl = v.objCtrl} ;

    -- Using nominal instead of sentential objects, e.g. "the fact that S"
    Compl3V2S v np =
      insertObjNP np v.c3 (predVc v) ** {cor = v.cor ; ext = []} ;
    Compl3V2V v np =
      insertObjNP np v.c3 (predVc v) ** {cor = v.cor ; ext = []} ;
    Compl3V2Q v np =
      insertObjNP np v.c3 (predVc v) ** {cor = v.cor ; ext = []} ;

  -- adverb with correlate (e.g. "dort" in "dort, wo der Pfeffer wächst")

    -- Now subsumed by AdvVP and ExtAdvVP:
    -- AdvCorVP vp adv = case adv.hasCor of {
    --   True => insertExtrapos (comma ++ adv.s ++ adv.cp) (insertAdv adv.cor vp) ;
    --   False => insertAdv (adv.s ++ adv.cp ++ adv.cor) vp
    -- } ;


-- "es wird gelacht"; generating formal sentences

  lincat
    FClause = ResGer.VP ** {subj : ResGer.NP} ;

  lin
    VPass v =
      let vp = predV werdenPass -- construct the formal clause
      in (insertObj (\\_ => v.s ! VPastPart APred) vp) ** {subj = esSubj} ;

    AdvFor adv fcl = fcl ** {a2 = fcl.a2 ++ adv.s} ;
	
    FtoCl cl =
      let subj = mkSubject cl.subj cl.c1
      in DisToCl subj.s subj.a (lin VP cl) ;

  oper -- extra operations for ExtraGer

    mkFoc : Str -> Cl -> Foc = \focus, cl ->
		lin Foc {s = \\m,t,a,p => focus ++ cl.s ! m ! t ! a ! p ! Inv} ;

    esSubj : CatGer.NP = lin NP {
      s = \\_,_ => "es" ;
      rc, ext = [] ;
      a = AgSgP3 Neutr ;
      w = WPron
    } ;

    DisToCl : Str -> Agr -> ResGer.VP -> Clause = \subj,agr,vp ->
	  let vps = useVP vp in {
      s = \\m,t,a,b,o =>
        let
          ord   = case o of {
            Sub => True ;  -- glue prefix to verb
            _ => False
            } ;
          verb  = vps.s  ! ord ! agr2vagr agr ! VPFinite m t a ;
          neg   = vp.a1 ++ negation ! b ; -- HL 8/19 vp.a1 ! b ;
          obj1  = (vp.nn ! agr).p1 ;
          obj2  = (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 ++ (vp.nn ! agr).p4 ;
          compl = obj1 ++ neg  ++ vp.adj ++ obj2 ++ vp.a2 ; -- adj added
          inf = vp.inf.inpl.p2 ++ verb.inf ;  -- not used for linearisation of Main/Inv
          infExt = vp.inf.extr ! agr ;
          extra = vp.ext ;
          inffin : Str =
            case <a,vp.isAux> of {                       
	           <Anter,True> => verb.fin ++ inf ; -- double inf   --# notpresent
                   _            => inf ++ verb.fin   --- or just auxiliary vp
            }                                            
        in
        case o of {
	    Main => subj ++ verb.fin ++ compl ++ infExt ++ verb.inf ++ extra ++ vp.inf.inpl.p2 ;
	    Inv  => verb.fin ++ compl ++ infExt ++ verb.inf ++ extra ++ vp.inf.inpl.p2 ; -- vp.inf.s ;
	    Sub  => compl ++ infExt ++ inffin ++ extra }
    		} ; 
		
		-- this function is not entirely satisfactory as largely 
		-- though not entirely duplicating mkClause in ResGer

  lincat
    SConj = {s : Order => Str} ;
  lin
    -- Construct sentences in conjunctive mood

    UseConjCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! MConjunct ! t.t ! t.a ! p.p ! o
      } ;

  lincat
    NS, NQ, NV = Noun ** {c2 : Preposition} ; -- to replace SentCN : CN -> SC -> CN
  oper
    mkCorN : Preposition -> Str = \p ->
      case p.t of {isContracting => p.s ! CAdvPron ; _ => "" } ;
  lin
    -- Constructions for sentential complementations of nouns
    UseNS ns = {
      s = \\_ => ns.s ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = ns.g
      } ;
    CorNS ns = {
      s = \\a,n,c => ns.s ! n ! c ++ (mkCorN ns.c2) ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = ns.g
      } ;
    ComplNS ns s = {
      s = \\a,n,c => ns.s ! n ! c ;
      rc = \\n => [] ;
      ext = embedInCommas (conjThat ++ s.s ! Sub) ;
      adv = [] ;
      g = ns.g
      } ;
    ComplConjNS ns s = {
      s = \\a,n,c => ns.s ! n ! c ;
      rc = \\n => [] ;
      ext = embedInCommas (s.s ! Main) ;  -- alternatively: s ! Main in conjunctive ?
      adv = [] ;
      g = ns.g
      } ;
    ComplCorNS ns s = {
      s = \\a,n,c => ns.s ! n ! c ++ (mkCorN ns.c2) ;
      rc = \\n => [] ;
      ext = embedInCommas (conjThat ++ s.s ! Sub) ;
      adv = [] ;
      g = ns.g
      } ;


    UseNV nv = {
      s = \\_ => nv.s ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = nv.g
      } ;
    CorNV nv = {   -- e.g. Interesse daran
      s = \\a,n,c => nv.s ! n ! c ++ (mkCorN nv.c2) ;
      rc = \\n => [] ;
      ext,adv = [] ;
      g = nv.g
      } ;
    ComplNV nv vp = {  -- e.g. Interesse , vp.infzu
      s = \\a,n,c => nv.s ! n ! c ;
      rc = \\n => [] ;
      ext = embedInCommas (useInfVP False vp) ;
      adv = [] ;
      g = nv.g
      } ;
    Compl2NV nv np = {  -- e.g. Interesse an einem Erfolg
      s = \\a,n,c => nv.s ! n ! c ;
      rc = \\n => [] ;
      ext = appPrep nv.c2 np ;
      adv = [] ;
      g = nv.g
      } ;
    ComplCorNV nv vp = {  -- e.g. Interesse daran , vp.infzu
      s = \\a,n,c => nv.s ! n ! c ++ (mkCorN nv.c2 ) ;
      rc = \\n => [] ;
      ext = embedInCommas (useInfVP False vp) ;
      adv = [] ;
      g = nv.g
      } ;

    UseNQ nq = {
      s = \\_ => nq.s ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = nq.g
      } ;
    CorNQ nq = {
      s = \\a,n,c => nq.s ! n ! c ++ (mkCorN nq.c2) ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = nq.g
      } ;
    ComplNQ ns q = {
      s = \\a,n,c => ns.s ! n ! c ;
      rc = \\n => [] ;
      ext = embedInCommas (q.s ! QIndir) ;
      adv = [] ;
      g = ns.g
      } ;
    Compl2NQ ns np = {
      s = \\a,n,c => ns.s ! n ! c ++ appPrep ns.c2 np ;
      rc = \\n => [] ;
      ext = [] ;
      adv = [] ;
      g = ns.g
      } ;
    ComplCorNQ ns q = {
      s = \\a,n,c => ns.s ! n ! c ++ (mkCorN ns.c2) ;
      rc = \\n => [] ;
      ext = embedInCommas (q.s ! QIndir) ;
      adv = [] ;
      g = ns.g
      } ;

{-    -- simpler alternative for binary noun with sentential complement ---
  lin
    SentN2 n2 sc = {
      s = \\a,n,c => n2.s ! n ! c ;
      rc = \\n => [] ;
      ext = embedInCommas sc.s ;
      adv = [] ;
      g = n2.g
    } ;
    SentCorN2 n2 sc = {
      s = \\a,n,c => n2.s ! n ! c ++ (mkCorN n2.c2) ;
      rc = \\n => [] ;
      ext = embedInCommas sc.s ;
      adv = [] ;
      g = n2.g
    } ;
    CorN2 n2 = {
      s = \\a,n,c => n2.s ! n ! c ++ (mkCorN n2.c2) ;
      rc = \\n => [] ;
      ext = [] ;
      adv = [] ;
      g = n2.g
    } ; ------------------------------------------------------------
-}

  -- To build adverb with correlate (e.g. "da" in "da, wo der Pfeffer wächst")

  lin
    dort_wo_Subj = P.mkSubj "dort" "wo" ; -- more in tests/german/TestLexicon

    -- Grammar rule (test)

  PositSentA2 a2 sc =
    let cor : Str = case a2.c2.t of {isContracting => a2.c2.s ! CAdvPron ; _ => []}
    in {
      s = \\a => cor ++ a2.s ! Posit ! a ;
      ext = comma ++ sc.s ;
      s2 = \\_ => [] ;
      c = <[], []> ;
      isPre = False
    } ;
  -- RAdvRCl iadv s = {s = \\rgn => iadv.s ++ s.s ! Sub} ;
  -- RelAdv is bad: accepts "die Frage , wo sie war" by ExtAdvNP with metavariable for cor = adv.s
  RelAdv adv qs = {s = qs.s ! QIndir ; cor = adv.s ; cp = [] ; hasCor,isClause = False} ;
  -- QuestIAdv : IAdv -> Cl -> QCl

  -- Adverb from predicative adjective in superlative: e.g. "am besten"
  SuperlAdvAdj adj = -- : A -> Adv ;
    {s = adj.s ! Superl ! APred ; cp,cor = [] ; hasCor,isClause = False} ;

  -- Adverb from infinitive (in addition to Extend.InOrderToVP)
  WithoutToVP vp = {
    s = "ohne" ++ useInfVP False vp ; cp,cor = [] ; hasCor,isClause = False} ;

  -- Location names
  AdjNSg adj n = {
    s = \\af,c => Predef.CAPIT ++ adj.s ! Posit ! (agrAdj af (gennum n.g Sg) c) ++ n.s ! Sg ! c ;
    hasDefArt = True ; g = n.g ; n = Sg} ;
  AdjNPl adj n = {
    s = \\af,c => Predef.CAPIT ++ adj.s ! Posit ! (agrAdj af (gennum n.g Pl) c) ++ n.s ! Pl ! c ;
    hasDefArt = True ; g = n.g ; n = Pl
    } ;
  CardLN card n = {
    s = \\af,c => Predef.CAPIT ++ card.s ! (agrAdj af (gennum n.g card.n) c) ++ n.s ! card.n ! c ;
    hasDefArt = True ; g = n.g ; n = card.n
    } ;
  OrdSgLN ord n = {
    s = \\af,c => Predef.CAPIT ++ ord.s ! (agrAdj af (gennum n.g Sg) c) ++ n.s ! Sg ! c ;
    hasDefArt = True ; g = n.g ; n = Sg
    } ;

} 
