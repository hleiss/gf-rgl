--# -path=.:../abstract:../common:../api:../prelude

abstract Refl =
   Grammar,
   -- ExtraGerAbs-[AdvRAP,AdvRNP,ReflA2RNP,ReflRNP] -- ReflRNP = ComplRSlash
   Extend[NP,Conj,Predet,Num,CN,VP,Cl,Tense, -- of which the following depend:
          RNP,RNPList,Base_nr_RNP,Base_rn_RNP,Base_rr_RNP,Cons_nr_RNP,Cons_rr_RNP,
          ConjRNP,ReflPron,ReflPoss,PredetRNP],
   Lexicon,ReflLexicon
   ** {
    -- HL 7/2023: To define reflexive predicates, like

    --    to love [oneself | one's children | the friends of one's children ] | to blow one's nose
    --    to live [in one's (own) house | in the house of one's parents | ...].
    --    to be [oneself | one's own boss | proud of oneself | proud of one's children |
    --           the president of one's university | the youngest of one's parents' children]

    -- we need not only reflexive noun phrases RNP, but also reflexive adjective phrases RAP, 
    -- reflexive adverbs RAdv and reflexive common nouns RCN, to build complements to copula verbs.

  cat
    RAP ; RAdv ; RCN ; -- RNP in Extend
    
  fun
    -- Constructions of RAP, RAdv, RCN and RNP
    
    ComplRA2 : A2 -> RNP -> RAP ;    -- proud of [oneself | one's children]  = ReflA2RNP
    ComparRA : A -> RNP -> RAP ;     -- older than [oneself | ...]
    CAdvRAP : CAdv -> AP -> RNP -> RAP ;  -- more stupid than [oneself | one's brother]

    PrepRNP : Prep -> RNP -> RAdv ;  -- (to live) in one's (own) house
    ComparRAdvAdj : CAdv -> A -> RNP -> RAdv ; -- (to do it) as cleverly as one's students

    ComplRN2 : N2 -> RNP -> RCN ;    -- relation to [oneself | one's child]
    ComplRN3 : N3 -> RNP -> RNP -> RCN ;    -- distance from one's home to one's office
    -- ComplR2N3 : N3 -> RNP -> NP -> RCN ; -- distance from one's home to Paris
    -- ComplR3N3 : N3 -> NP -> RNP -> RCN ; -- distance from Paris to one's home

    PossRNP : CN -> RNP -> RCN ;     -- (a) room of [one's own | a friend of one's parents]
    AdjRCN : RAP -> CN -> RCN ;      -- older person than one's children
    
    -- Imported from Extend:
    -- ReflPron : RNP ;                   -- myself
    -- PredetRNP : Predet -> RNP -> RNP ; -- all my brothers
    -- ReflPoss : Num -> CN -> RNP ;      -- [my | one's | one's neighbour's] car(s)
    DetRCN : Det -> RCN -> RNP ;
    
    -- Reflexive complements of copula verbs come from reflexive arguments:

    CompRAP : RAP -> Comp ;    -- (to be) older than one's sister
    CompRAdv : RAdv -> Comp ;  -- (to be) in one's youth
    CompRCN : RCN -> Comp ;    -- (to be) more intelligent than oneself
    CompRNP : RNP -> Comp ;    -- (to be) the child of one's friend

    -- Reflexive nominal and adjectival complements can be inserted into Ger.nn : Agr => Str*..*Str
    -- of VP and VPSlash and then depend on the missing subject's agreement:

    SlashR2V3 : V3 -> RNP -> VPSlash ;    -- introduce one's friend to sb
    SlashR3V3 : V3 -> RNP -> VPSlash ;    -- introduce sb to one's friend
    ComplRSlash : VPSlash -> RNP -> VP ;  -- = Extra.ReflRNP 
    ComplRVA : VA -> RAP -> VP ;          -- become older than one's father

    -- In Eng, sentence adverbs VP.ad:Agr => Str depend on the subject's agreement,
    -- in Ger and several other languages, VP.a2:Str doesn't. Maybe it should. 

    -- Hence, we can only add radv:RAdv into Ger.VPSlash.a2:Str, if we simultaneously add
    -- a nominal object radv depends on, i.e. insert the string radv.s ! np.a into VPSlash.a2:
    -- e.g. "to visit someone in his house". If at all, how far do we want to control such
    -- dependencies of adverbs from nominal objects via possessives???
    
    ComplSlashRAdv : VPSlash -> NP -> RAdv -> VP ;     -- RNP has no agreement RNP.a (yet)

    -- Likewise we cannot insert a reflexive adverb into VP.a2 : Str, but add
    -- the string RAdv.s ! NP.a to VP.a2 : Str and NP.s as subject:
    
    PredVPRAdv : NP -> VP -> RAdv -> Cl ;  -- er lebt in [seinem Haus | dem Haus seiner Frau]

    -- (Generic) Reflexive predicates

    -- We may improve GenericCl : VP -> Cl by adding the indefinite
    -- personal pronoun one_Pron instead of "one" in english/IdiomEng
    -- and "man" in german/IdiomGer, or by just adding a new agreement
    -- value AgSgP3Gen in ResEng/Ger to resolve reflexive personal and
    -- possessive pronouns as "oneself"/"sich" and "one's"/"sein".
    --
    -- Adding one_Pron causes unnecessary ambiguities for Ger for
    -- possessive usage: one's car = sein Wagen = his car.  Another
    -- difference is: to close one's eyes = die(=die eigenen) Augen
    -- schließen

    -- IdiomGer: GenericCl vp = mkCl "man" AgSgP3Gen vp ;

   -- To ease parsing examples of different categories in gfs-scripts:

   UttRAP : RAP -> Utt ;
   UttRAdv : RAdv -> Utt ;
   UttRCN : RCN -> Utt ;
   UttRNP : RNP -> Utt ;
   UttVPSlash : VPSlash -> Utt ;
    
}
