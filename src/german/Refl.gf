--# -path=.:../abstract:../common:../api:../prelude

abstract Refl =
--   Cat,Noun,Adjective,Structural,Verb,Sentence,
   Grammar,
   Lexicon,ReflLexicon,
   ExtraGerAbs-[AdvRAP,ReflA2RNP,AdvRNP,ReflRNP] -- ReflRNP = ComplRSlash
   ** {
    -- HL 7/2023: To define reflexive predicates, like

    --    to love [oneself | one's children | the friends of one's children ] | to blow one's nose
    --    to live [in one's (own) house | in the house of one's parents | ...].
    --    to be [oneself | one's own boss | proud of oneself | proud of one's children |
    --           the president of one's university | the youngest of one's parents' children]

    -- we need not only reflexive noun phrases RNP, but also reflexive adjective phrases RAP, 
    -- reflexive adverbs RAdv and reflexive common nouns RCN, to build complements to copula verbs.

  cat
    RAP ; RAdv ; RCN ; -- RNP in ExtraGer
    
  fun
    -- Constructions of RAP, RAdv, RCN and RNP
    
    ComplRA2 : A2 -> RNP -> RAP ;    -- proud of [oneself | one's children]  = ReflA2RNP
    ComparRA : A -> RNP -> RAP ;     -- older than [oneself | ...]
    CAdvRAP : CAdv -> AP -> RNP -> RAP ;  -- more stupid than [oneself | one's brother]

    PrepRNP : Prep -> RNP -> RAdv ;  -- (to live) in one's (own) house

    ComplRN2 : N2 -> RNP -> RCN ;    -- mother of [oneself | one's child]
    -- ComplRN3 : N3 -> RNP -> RNP -> RCN ; 
    -- ComplR2N3 : N3 -> RNP -> NP -> RCN ; 
    -- ComplR3N3 : N3 -> NP -> RNP -> RCN ; 

    PossRNP : CN -> RNP -> RCN ;     -- (a) room of [one's own | a friend of one's parents]

    -- ExtraGer has:
    -- ReflPoss : Num -> CN -> RNP ; -- [my | one's | one's neighbour's] family 
    DetRCN : Det -> RCN -> RNP ;
    
    -- Reflexive complements of copula verbs come from reflexive arguments:

    CompRAP : RAP -> Comp ;    -- or RComp.ext : Agr => Str: older than one's sister ?
    CompRAdv : RAdv -> Comp ;
    CompRCN : RCN -> Comp ;
    CompRNP : RNP -> Comp ;

    -- Reflexive noun phrases can be inserted into nn : Agr => Str*..*Str of VP and VPSlash
    -- and then depend on the missing subject's agreement:

    SlashR2V3 : V3 -> RNP -> VPSlash ;
    SlashR3V3 : V3 -> RNP -> VPSlash ;
    ComplRSlash : VPSlash -> RNP -> VP ;  -- = Extra.ReflRNP 
    ComplRVA : VA -> RAP -> VP ;         -- es blauer als sein Haus malen
    
    -- Reflexive adverbs cannot be inserted into VPSlash.a2:Str, but we can
    -- add them simultaneously with a noun phrase they depend on: we insert 
    -- the string(!) RAdv.s ! NP.a into VPSlash.a2 and NP.s as object: 
    
    ComplSlashRAdv : VPSlash -> NP -> RAdv -> VP ;     -- RNP has no agreement RNP.a

    -- Likewise we cannot insert a reflexive adverb into VP.a2 : Str, but add
    -- the string RAdv.s ! NP.a to VP.a2 : Str and NP.s as subject:
    
    PredVPRAdv : NP -> VP -> RAdv -> Cl ;  -- er lebt in [seinem Haus | dem Haus seiner Frau]

    -- Reflexive (or: generic) predicates

-- We need to improve GenericCl : VP -> Cl by adding the indefinite personal pronoun one_Pron
-- instead of "one" in english/IdiomEng and "man" in german/IdiomGer). Does it need its own
-- agreement values Ag g n P0 to resolve reflexive possessives as "one's" instead of "its"
-- (and "sein eigen-er" instead of "sein")? Adding one_Pron seems fine for Eng, but causes
-- ambiguities for Ger for possessive usage: one's car = sein Wagen = his car.
-- Another difference is: to close one's eyes = die(=die eigenen) Augen schließen
-- But: we could use (Ag g n P0) *only* in GenericCl and in ReflPoss, but *without* one_Pron.

--    Do we have to add an indefinite pronoun one_Pron = "man" and use it to parse
--    reflexive predicates? No, suitable agreement in GenericCl suffices
--    Pron one_Pron gives ambiguities and could incorrectly be used as object.

-- man liebt [sich | seine Kinder]
                            -- man kann sich nicht alles gefallen lassen
                            -- man muß sein Bestes geben       
                            -- es kann das eigene Leben kosten [it may cost [one's | your] own life]
    -- RNP: the duty to love one's wife and children
    --      * the duty to love your wife
    -- VP:  to care about the children of one's friends
    --      to wash one's hands
    
    -- Cl:  one has to do one's best
    --      one can easily lose one's way
    --      one cannot always help oneself                       (ReflPron object - without new one_Pron!)
    --      it is not allowed to touch the ball with one's hand  (RAdv)
    --      it is unpolite to blow one's nose in the tablecloth  (RNP object)
    
{- which CN's need a reflexive version?

+ fun PossNP : CN -> NP -> CN ;
+ fun AdjCN : AP -> CN -> CN ;
fun AdvCN : CN -> Adv -> CN a ;

fun ApposCN : CN -> NP -> CN ;
+ fun ComplN2 : N2 -> NP -> CN ;
fun RelCN : CN -> RS -> CN ;

-}
}
