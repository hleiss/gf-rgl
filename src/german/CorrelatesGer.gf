--# -path=.:../abstract:../common:../api:../prelude

concrete CorrelatesGer of CorrelatesGerAbs = CatGer ** 
  open ResGer, Coordination, Prelude, IrregGer, (P = ParadigmsGer), (N = NounGer) in {
  flags coding=utf8 ;

  -- Sentential complement with correlate

  oper
    mkCor : Preposition -> Str = \p ->
      case p.t of {isPrep => p.s ! CAdvPron ; _ => "es" } ; -- | "das"} ;
    corVP : Verb -> Preposition -> ResGer.VP = \v,p ->
      let cor : Str = mkCor p ;
          vp = predV v ;
          vnn = vp.nn
      in case p.t of {
        isCase => vp ** {nn = \\a => <cor ++ (vnn!a).p1, (vnn!a).p2, (vnn!a).p3, (vnn!a).p4>} ;
        _      => vp ** {nn = \\a => <(vnn!a).p1, (vnn!a).p2, cor ++ (vnn!a).p3, (vnn!a).p4>}
      } ** {lock_VP = <>} ;
    corVPSlash : Verb ** {c2:Preposition} -> Preposition -> ResGer.VPSlash = \v,p ->
      let cor : Str = mkCor p ;
          vp = predVc v ;
          vnn = vp.nn
      in case p.t of {
        isCase => vp ** {nn = \\a => <cor ++ (vnn!a).p1, (vnn!a).p2, (vnn!a).p3, (vnn!a).p4>} ;
        _      => vp ** {nn = \\a => <(vnn!a).p1, (vnn!a).p2, cor ++ (vnn!a).p3, (vnn!a).p4>}
      } ** {lock_VPSlash = <>} ;

  lin
    -- correlate for sentential subject (SC)
    CorPredSCVP sc vp = mkClause "es" (agrP3 Sg) (insertExtrapos sc.s vp) ;
    -- CorSCVP vp = ImpersCl vp ;

    -- correlate for sentential|infinitival object is like a pronominal or prepositional object
    CorComplVS v s =                     -- (nicht) daran glauben, dass S
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (corVP v v.c2) ;
    CorVS v = corVP v v.c2 ;
    CorComplVQ v q =                       -- danach fragen, ob S
      insertExtrapos (comma ++ q.s ! QIndir) (corVP v v.c2) ;
    CorVQ v = corVP v v.c2 ;

  -- correlate for infinitival object           TODO: vfin ++ cor ++ adv ++ neg ++ vinf
  -- lintype VV now has c2:Preposition;  denke daran, .. zu tun | will *es tun
    CorComplVV vv vp =                       -- generalizes former EsVV
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
          vvp = corVP vv vv.c2
      in
      insertExtrapos vp.ext (insertInf inf vvp) ;
    CorVV v = corVP v v.c2 ;

    -- TODO: use v.objCtrl to exclude es-correlate for vv.isAux=True: will|kann|muss *es schlafen
    --       wir versprechen|lassen|helfen *es euch , infzu
    
    CorSlashV2S v s = (corVPSlash v v.c3) ** {ext = comma ++ conjThat ++ s.s ! Sub} ;
    CorSlashV2Q v q = (corVPSlash v v.c3) ** {ext = comma ++ q.s ! QIndir} ;
    CorV2S v = corVPSlash v v.c3 ;
    CorV2Q v = corVPSlash v v.c3 ;

    CorV2V v =
      let cor : Str = mkCor v.c3 ;
          vps = predVGen v.isAux v -- e.g. verspricht|bittet.isAux=False | läßt.isAux=True
                  ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;
          vnn = vps.nn
      in case v.c3.t of {
        isCase => vps ** {nn = \\a => <cor ++ (vnn!a).p1, (vnn!a).p2, (vnn!a).p3, (vnn!a).p4>} ;
        _      => vps ** {nn = \\a => <(vnn!a).p1, (vnn!a).p2, cor ++ (vnn!a).p3, (vnn!a).p4>}
      } ;
    CorSlashV2V v vp =             -- bitte (jmdn) darum , zu kommen
      let
        vps = CorV2V v ;
        inf = mkInf v.isAux Simul Pos vp
      in
      insertExtrapos vp.ext (
        insertInf inf vps) ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;

-- Interrogatve correlate for sentential object
-- ICorVS : VS -> QVP ;          -- woran glauben
-- PredIQVP : QVP -> NP -> QCl ; -- woran glauben die Kinder

    -- Sentences in conjunctive mood
  lincat
    SConj = {s : Order => Str} ;
  lin
    UseConjCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! MConjunct ! t.t ! t.a ! p.p ! o
      } ;

    -- Noun with sentential complement (to replace SentCN : CN -> SC -> CN)
  lincat
    NS, NQ, NV = Noun ** {c2 : Preposition} ; -- to replace SentCN : CN -> SC -> CN
  oper
    mkCorN : Preposition -> Str = \p ->
      case p.t of {isPrep => p.s ! CAdvPron ; _ => "" } ;
  lin
    -- Constructions for sentential complementations of nouns
    UseNS ns = {
      s = \\_ => ns.s ;
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
    CorNS ns = {
      s = \\a,n,c => ns.s ! n ! c ++ (mkCorN ns.c2) ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = ns.g
      } ;
    CorComplNS ns s = {
      s = \\a,n,c => ns.s ! n ! c ++ (mkCorN ns.c2) ;
      rc = \\n => [] ;
      ext = embedInCommas (conjThat ++ s.s ! Sub) ;
      adv = [] ;
      g = ns.g
      } ;
    Compl2NS ns np = {
      s = \\a,n,c => ns.s ! n ! c ++ appPrep ns.c2 np ;
      rc = \\n => [] ;
      ext = [] ;
      adv = [] ;
      g = ns.g
      } ;

    UseNV nv = {
      s = \\_ => nv.s ;
      rc = \\_ => [] ;
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
    CorNV nv = {   -- e.g. Interesse daran
      s = \\a,n,c => nv.s ! n ! c ++ (mkCorN nv.c2) ;
      rc = \\n => [] ;
      ext,adv = [] ;
      g = nv.g
      } ;
    CorComplNV nv vp = {  -- e.g. Interesse daran , vp.infzu
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
    CorNQ nq = {
      s = \\a,n,c => nq.s ! n ! c ++ (mkCorN nq.c2) ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = nq.g
      } ;
    CorComplNQ ns q = {
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

  -- Adjective with sentential complement (replace modification SentAP : AP -> SC -> AP)

    -- simpler alternative for binary adjective with sentential object

  SentA2 a2 sc =
    {
      s = \\a => a2.s ! Posit ! a ;
      ext = comma ++ sc.s ;
      s2 = \\_ => [] ;
      c = <[], []> ;
      isPre = False -- True?
    } ;
  CorSentA2 a2 sc =
    let cor : Str = case a2.c2.t of {isPrep => a2.c2.s ! CAdvPron ; _ => []}
    in {
      s = \\a => cor ++ a2.s ! Posit ! a ;
      ext = comma ++ sc.s ;
      s2 = \\_ => [] ;
      c = <[], []> ;
      isPre = True
    } ;
  CorA2 a2 =
    let cor : Str = case a2.c2.t of {isPrep => a2.c2.s ! CAdvPron ; _ => []}
    in {
      s = \\a => cor ++ a2.s ! Posit ! a ;
      ext = [] ;
      s2 = \\_ => [] ;
      c = <[], []> ;
      isPre = True
    } ;


-- extra rules to get some of the "es" alternative linearisations

  lincat
    VSA = Verb ;

  lin
    EsV2A v2a ap s = predV v2a ** {
      nn = \\_ => <"es",[],[],[]> ;
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;

    CorVSA vsa ap = predV vsa ** {          -- TODO: test and document
      adj = ap.s ! APred} ;
    ComplVSA vsa s ap = predV vsa ** {
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;
    CorComplVSA vsa s ap = predV vsa ** {   -- EsV2A
      nn = \\_ => <"es",[],[],[]> ;
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;
    CorComplVVA vsa vp ap =
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
      in
      insertExtrapos vp.ext (
        insertInf inf (predV vsa ** { nn = \\_ => <"es",[],[],[]> ;
                                      adj = ap.s ! APred } -- ++ ap.s2 ...
          )) ;



  -- adverb with correlate (e.g. "dort" in "dort, wo der Pfeffer wächst")

    -- Now subsumed by AdvVP and ExtAdvVP:
    -- AdvCorVP vp adv = case adv.hasCor of {
    --   True => insertExtrapos (comma ++ adv.s ++ adv.cp) (insertAdv adv.cor vp) ;
    --   False => insertAdv (adv.s ++ adv.cp ++ adv.cor) vp
    -- } ;

}
