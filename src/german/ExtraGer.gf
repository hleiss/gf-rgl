--# -path=.:../abstract:../common:../api:../prelude:

concrete ExtraGer of ExtraGerAbs = CatGer **
  open ResGer, Coordination, Prelude, IrregGer, (P = ParadigmsGer), (N = NounGer), (S = StructuralGer) in {

  flags coding=utf8 ;

  lin
    PPzuAdv cn = {
      s = case cn.g of {
        Masc | Neutr => "zum" ;
        Fem => "zur"
        } ++ cn.s ! adjfCase Weak Dat ! Sg ! Dat ;
      cp,cor =[] ; hasCor,t = False
    } ;

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

  lin
    EsVV vv vp =                             -- HL 3/2022
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
          objs : Agr => Str * Str * Str * Str = \\a => <"es",[],[],[]> ;
          vps = predV vv ** { nn = objs }
      in insertExtrapos vp.ext (
           insertInf inf vps) ;

    EsV2A v2a ap s = predV v2a ** {
      nn = \\_ => <"es",[],[],[]> ;
      adj = ap.s ! APred ;
      ext = comma ++ conjThat ++ s.s ! Sub} ;

  -- Sentential complement with correlate

  oper
    mkCor : Preposition -> Str = \p ->
      case p.t of {isContracting => p.s ! CAdvPron ; _ => "es" | "das"} ;
  lin
    CorVS vs =
      predV vs ** {c2 = vs.c2 ; cor = mkCor vs.c2} ;
    ComplCorVS vs s =
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub)
      (predV vs ** {c2 = vs.c2 ; cor = mkCor vs.c2}) ;

  -- Infinitival complement with correlate 1/2025
  -- lintype VV now has c2:Preposition and cor:Str;  denke daran, .. zu tun | will es tun
    CorVV vv =
      predV vv ** {c2 = vv.c2 ; cor = mkCor vv.c2} ;
    ComplCorVV vv vp =
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
      in
      insertExtrapos vp.ext (insertInf inf (CorVV vv)) ;
    -- todo: position of correlate:
    -- p -cat=Cl "ich denke [daran] jeden Montag [daran], dein Buch zu lesen"

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
    NS, NQ, NV = Noun ** {c2 : Preposition} ; -- to replace SentCN -> SC -> CN
  lin
    -- Constructions for sentential complementations of nouns
{-
    SentN2 n2 sc =
      let cor : Str = case n2.c2.t of {isContracting => n2.c2.s ! CAdvPron ; _ => []} ;
      in {
        s = \\_,n,c => n2.s ! n ! c ++ cor ++ comma ;
        ext = sc.s ;
        rc = \\_ => [] ;
        adv = [] ;
        g = n2.g
      } ;
-}
    UseNS ns = {
      s = \\_ => ns.s ;
      rc = \\_ => [] ;
      ext,adv = [] ;
      g = ns.g
      } ;
    ComplNS ns s =
      let p = ns.c2 ;
          cor = case p.t of {isContracting => p.s ! CAdvPron ; _ => []}
      in {
        s = \\a,n,c => ns.s ! n ! c ++ cor ++ comma ;
        rc = \\n => [] ;
        ext = conjThat ++ s.s ! Sub ; -- alternatively: s ! Main in conjunctive ?
        adv = [] ;
        g = ns.g
      } ;
    ComplConjNS ns s = {
        s = \\a,n,c => ns.s ! n ! c ++ comma ;
        rc = \\n => [] ;
        ext = s.s ! Main ;
        adv = [] ;
        g = ns.g
      } ;


    -- ComplNQ : NQ -> QS -> CN ;
    -- ComplNV : NV -> VP -> CN ;

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
  -- RelAdv is bad: accepts "die Frage , wo sie war" by ExtAdvNP with metavariable for cor = []
  RelAdv adv qs = {s = qs.s ! QIndir ; cor = adv.s ; cp = [] ; hasCor,t = False} ;

} 
