abstract ExtraGerAbs = Extra [
    Tense, Temp, Pol, Conj, Prep, Num, Det, Predet, Quant, CN, NP, AP,
      VV, VP, S, Adv, IAdv, IComp, IQuant, Cl, ClSlash, RCl, Utt,
      Foc, FocObj, FocAdv, FocAP, FocNeg, FocVP, FocVV, UseFoc
  ] ** {
  flags coding=utf8;
  
  cat
	FClause ; -- formal clause 
  fun
    PPzuAdv   : CN -> Adv ;  -- zum Lied, zur Flasche
    TImpfSubj : Tense ;      -- ich möchte...   --# notpresent

    moegen_VV : VV ;         -- ich mag/möchte singen

	EsVV : VV -> VP -> VP ; -- ich genieße es zu schlafen
 	EsV2A : V2A -> AP -> S -> VP ; -- ich finde es schön, dass ...

  	VPass : V -> FClause ;   -- (es) wird getanzt
  	AdvFor : Adv -> FClause -> FClause ; -- es wird heute gelacht - addition of adverbs
  	FtoCl : FClause -> Cl ;  -- embedding FClause within the RGL, to allow generation of S, Utt, etc.

    Pass3V3 : V3 -> VPSlash ; -- wir bekommen den Beweis erklärt

}
