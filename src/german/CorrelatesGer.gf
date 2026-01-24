--# -path=.:../abstract:../common:../api:../prelude

concrete CorrelatesGer of Correlates = CatGer, ExtraGer[Foc, FocObj, UseFoc] **
  open ResGer, Coordination, Prelude, IrregGer, (P = ParadigmsGer), (N = NounGer) in {
  flags coding=utf8 ;

  -- Sentential complement with correlate

  oper
    demPron : Case => Str =
      table {Dat => "dem" ; Gen => "dessen" ; _ => "es"} ; -- | "das"} ;
    mkCor : Preposition -> Str = \p ->
      case p.t of {isPrep => p.s ! CAdvPron ; _ => demPron ! p.c} ;

    insertCor : Preposition -> ResGer.VP -> ResGer.VP = \p,vp ->
      let cor : Str = CorrelatesGer.mkCor p ;
          vnn = vp.nn
      in case p.t of {
        isCase => vp ** {nn = \\a => <cor ++ (vnn!a).p1, (vnn!a).p2, (vnn!a).p3, (vnn!a).p4>} ;
        _      => vp ** {nn = \\a => <(vnn!a).p1, (vnn!a).p2, cor ++ (vnn!a).p3, (vnn!a).p4>}
      } ;
    insertCorc : Preposition -> ResGer.VPSlash -> ResGer.VPSlash = \p,vp -> vp ** insertCor p vp ;
    -- inCommas : Str -> Str = embedInCommas ;
    inCommas : Str -> Str = \s -> bindComma ++ s ; -- to ease parse -cat=NP
  lin
    -- correlate for sentential subject (SC)
    CorPredSCVP sc vp = mkClause "es" (agrP3 Sg) (insertExtrapos (comma ++ sc.s) vp) ;
    -- CorSCVP vp = ImpersCl vp ;

    -- correlate for sentential|interrogative object is like a pronominal or prepositional object
    CorVS v = insertCor v.c2 (predV v) ;
    CorComplVS v s =                       -- glaube (nicht) daran , dass S
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (insertCor v.c2 (predV v)) ;

    CorVQ v = insertCor v.c2 (predV v);
    CorComplVQ v q =                       -- frage (nicht) danach , ob S
      insertExtrapos (comma ++ q.s ! QIndir) (insertCor v.c2 (predV v)) ;

  -- correlate for infinitival object           TODO: vfin ++ cor ++ adv ++ neg ++ vinf
    CorVV v = insertCor v.c2 (predV v);    -- will es (nicht) ; denke (nicht) daran
    CorComplVV v vp =                        -- no correlates for modal verb v.isAux=True
      let inf = mkInf v.isAux Simul Pos vp ; -- will *es schlafen ; denke daran, zu schlafen
          vvp = case v.isAux of {True => predV v ; _ => insertCor v.c2 (predV v)}
      in
      insertExtrapos vp.ext (insertInf inf vvp) ;

    -- -- VS, VQ, VV with nominal object  -- replace by the more general rules that follow
    -- Compl2VS vs np =
    --   let vp = (predVc vs ** {objCtrl = False})
    --   in insertObjNP np vs.c2 vp ;
    -- Compl2VQ vq np =
    --   let vp = (predVc vq ** {objCtrl = False})
    --   in insertObjNP np vq.c2 vp ;
    -- Compl2VV vq np =
    --   let vp = (predVc vq ** {objCtrl = False})
    --   in insertObjNP np vq.c2 vp ;

    -- For nominal instead of sentential objects, and questions like "what do you know|ask|intend?"
    UseVS v = v ;
    UseVQ v = v ;
    UseVV v = v ;

    -- Ternary verbs with correlate for sentential object
    CorV2S v = insertCorc v.c3 (predVc v) ;
    CorSlashV2S v s = insertCorc v.c3 (predVc v) ** {ext = comma ++ conjThat ++ s.s ! Sub} ;
    CorV2Q v = insertCorc v.c3 (predVc v) ;
    CorSlashV2Q v q = insertCorc v.c3 (predVc v) ** {ext = comma ++ q.s ! QIndir} ;

    -- But:  wir versprechen (es) euch, vp.infzu ; -- set cor=[] for c2=accPrep
    CorV2V v =                     -- e.g. verspricht|bittet.isAux=False | läßt.isAux=True
      insertCorc v.c3 (predVGen v.isAux v ** {c2 = v.c2 ; objCtrl = v.objCtrl}) ;

    CorSlashV2V v vp =             -- bitte (jmdn) darum, zu kommen ; helfe *es euch, zu leben
      let                          -- lasse (jmdn) *es schlafen ; but: rate ?es euch , zu kommen
        inf = mkInf v.isAux Simul Pos vp ;
        vps0 = predVGen v.isAux v ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;
        vps = case v.c3.t of {isCase => vps0 ;
                              _ => case v.isAux of {True => vps0 ; _ => insertCorc v.c3 vps0}}
      in
      insertExtrapos vp.ext (
        insertInf inf vps) ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;

    UseV2S v = v ;  -- V2S with two nominal objects, e.g. glaube dir deine Versprechen
    UseV2Q v = v ;
    UseV2V v = v ;

  -- Interrogatve (and relative) correlate for sentential complement
  oper
    quPron : Case => Str =
      table {Dat => "wem" ; Gen => "wessen" ; _ => "was"} ;
    mkICor : Preposition -> Str = \p ->
      case p.t of {isPrep => p.s ! CIPron ; _ => quPron ! p.c } ;

  -- Verb phrase and clause missing a sentential object
  lincat
    VPSlashS, VPSlashQS, VPSlashVP = ResGer.VPSlash ;
    ClSlashS, ClSlashQS, ClSlashVP = ResGer.Clause ** {c2 : Preposition} ; -- ClSlash
  lin
    SlashVSa, SlashVQa, SlashVVa = \vsc -> predVc vsc ;

    Slash2V2S, Slash2V2Q, Slash2V2V = \v2sc, np ->
      insertObjNP np v2sc.c2 (predVc v2sc) ** {c2 = v2sc.c3} ;

    AdvVPSlashS, AdvVPSlashQS, AdvVPSlashVV =
      \vp, adv -> vp ** insertAdv adv.s vp ;

    SlashVPSlashS, SlashVPSlashQS, SlashVPSlashVP = \np,vp ->
      let sb = mkSubject np vp.c1 in mkClause sb.s sb.a vp ** {c2 = vp.c2} ;

    -- Interrogative/relative clause with fronted sentential correlate "was", "woran" etc.

    QuestSlashS, QuestSlashQS, QuestSlashVP = \cls -> {
      s = \\m,t,a,p =>
        let cl = cls.s ! m ! t ! a ! p ;
            what = mkICor cls.c2
        in table {
          QDir   => what ++ cl ! Inv ;
          QIndir => what ++ cl ! Sub }
      } ;

    RelSlashS, RelSlashQS, RelSlashVP = \cls ->
      let what = mkICor cls.c2 in {
      s = \\m,t,a,p,gn => what ++ cls.s ! m ! t ! a ! p ! Sub ;
      c = cls.c2.c
      } ;

  -- Leftextraction of sentential object and correlate
  oper
    demPronLeft : Case => Str = table {Dat => "dem" ; Gen => "dessen" ; _ => "das"} ;
    mkCorLeft : Preposition -> Str = \p ->
       case p.t of {isPrep => p.s ! CAdvPron ; _ => demPronLeft ! p.c} ;

    insertLeft : Str -> Str -> ResGer.Clause ** {c2 : Preposition} -> Foc =
      \obj,cor,cl -> lin Foc {s = \\m,t,a,p => obj ++ cor ++ cl.s  ! m ! t ! a ! p ! Inv} ;
  lin
    FocS s cl   = insertLeft (conjThat ++ s.s ! Sub ++ comma) [] cl ;
    FocCorS s cl = insertLeft (conjThat ++ s.s ! Sub ++ comma) (mkCorLeft cl.c2) cl ;
    CorFocS cls = insertLeft [] (mkCorLeft cls.c2) cls ;
      
    FocQS qs cl = insertLeft (qs.s ! QIndir ++ comma) [] cl ;
    FocCorQS qs cl = insertLeft (qs.s ! QIndir ++ comma) (mkCorLeft cl.c2) cl ;
    CorFocQS cl = insertLeft [] (mkCorLeft cl.c2) cl ;
    
    -- TODO: no leading comma in inf; agr in inf?
    FocVP vp cl = let inf = mkInf False Simul Pos vp in --{inpl:(Agr=>Str)*Str; extr:Agr=>Str}
      insertLeft (inf.extr ! agrP3 Sg ++ vp.ext ++ comma) [] cl ;
    FocCorVP vp cl = let inf = mkInf False Simul Pos vp in
      insertLeft (inf.extr ! agrP3 Sg ++ vp.ext ++ comma) (mkCorLeft cl.c2) cl ;
    CorFocVP cl = insertLeft [] (mkCorLeft cl.c2) cl ;
    
    -- Sentences in conjunctive mood
  lincat
    SConj = {s : Order => Str} ;
  lin
    UseClConj t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s ! MConjunct ! t.t ! t.a ! p.p ! o
      } ;

    -- Noun with sentential complement (to replace SentCN : CN -> SC -> CN)
  lincat
    NS, NQ, NV = Noun ** {c2 : Preposition} ; -- to replace SentCN : CN -> SC -> CN
  oper
    mkNCor : Preposition -> Str = \p ->
      case p.t of {isPrep => p.s ! CAdvPron ; _ => "" } ;
  lin
    -- Constructions for sentential complementations of nouns
    ComplNS ns s = {
      s = \\_ => ns.s ;
      rc = \\n => [] ;
      ext = inCommas (conjThat ++ s.s ! Sub) ;
      adv = [] ;
      g = ns.g
      } ;
    ComplNSConj ns s = {
      s = \\_ => ns.s ;
      rc = \\n => [] ;
      ext = inCommas (s.s ! Main) ;  -- object sentence in conjunctive
      adv = [] ;
      g = ns.g
      } ;
    CorComplNS ns s = {
      s = \\_ => ns.s ;
      rc = \\n => [] ;
      ext = mkNCor ns.c2 ++ inCommas (conjThat ++ s.s ! Sub) ;
      adv = [] ;
      g = ns.g
      } ;
    -- Compl2NQ, 
    Compl2NS, Compl2NV = \ns,np -> {
      s = \\_ => ns.s ;
      rc = \\n => [] ;
      ext = appPrep ns.c2 np ; -- nominal instead of sentential object
      adv = [] ;
      g = ns.g
      } ;
    CorNS, CorNV, CorNQ = \ns -> {
      s = \\_ => ns.s ;
      rc = \\_ => [] ;
      ext = mkNCor ns.c2 ;  -- separate from ns: Glaube (der Leute) daran 
      adv = [] ;
      g = ns.g
      } ;
    
    ComplNV nv vp = {  -- e.g. Interesse , vp.infzu
      s = \\_ => nv.s ;
      rc = \\n => [] ;
      ext = inCommas (useInfVP False vp) ;
      adv = [] ;
      g = nv.g
      } ;
    CorComplNV nv vp = {  -- e.g. Interesse daran , vp.infzu
      s = \\_ => nv.s ;
      rc = \\n => [] ;
      ext = mkNCor nv.c2 ++ inCommas (useInfVP False vp) ;
      adv = [] ;
      g = nv.g
      } ;
    
    ComplNQ nq q = {
      s = \\a,n,c => nq.s ! n ! c ;
      rc = \\n => [] ;
      ext = inCommas (q.s ! QIndir) ;
      adv = [] ;
      g = nq.g
      } ;
    CorComplNQ nq q = {
      s = \\_ => nq.s ;
      rc = \\n => [] ;
      ext = mkNCor nq.c2 ++ inCommas (q.s ! QIndir) ;
      adv = [] ;
      g = nq.g
      } ;
    UseNQ nq = nq ;  -- instead of Compl2NQ

  -- Adjective with sentential complement (replace modification SentAP : AP -> SC -> AP)

  oper
    insertACor : Preposition -> Str * Str = \p ->
      let cor : Str = CorrelatesGer.mkCor p ;
      in case p.t of {isCase => <cor,[]> ; _ => <[],cor>} ;

  lincat
    AS = Adjective ** {c2:Preposition} ;
    AV = Adjective ** {c2:Preposition} ;
    AQ = Adjective ** {c2:Preposition} ;
  lin
    ComplAS as s = {
      s = as.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
      c = <[],[]> ;
      ext = embedInCommas (conjThat ++ s.s ! Sub)
      } ;
    ComplAQ aq qs = {
      s = aq.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
      c = <[],[]> ;
      ext = embedInCommas (qs.s ! QIndir)
      } ;
    ComplAV av vp = {
      s = av.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
      c = <[],[]> ;
      ext = embedInCommas (useInfVP False vp)
      } ;

    CorComplAS as s =
      let cor = CorrelatesGer.mkCor as.c2 in
      {s = as.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
       c = insertACor as.c2 ;
       ext = embedInCommas (conjThat ++ s.s ! Sub)
      } ;
    CorComplAQ aq qs =
      {s = aq.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
       c = insertACor aq.c2 ;
       ext = embedInCommas (qs.s ! QIndir)
      } ;
    CorComplAV av vp =
      {s = av.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
       c = insertACor av.c2 ;
       ext = embedInCommas (useInfVP False vp)
      } ;

    CorAS, CorAV, CorAQ = \as -> 
      {s = as.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
       c = insertACor as.c2 ;
       ext = []
      } ;
    Compl2AS, Compl2AV, Compl2AQ = \as,np ->
      let obj:Str = appPrep as.c2 np in 
      {s = as.s ! Posit ; s2 = \\_ => [] ; isPre = True ;
       c = case as.c2.t of {isCase => <obj,[]> ; _ => <[],obj>} ;
       ext = []
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
