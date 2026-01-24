--# -path=.:../abstract:../common:../api:../prelude

abstract Correlates = Cat, ExtraGerAbs[Foc, FocObj, UseFoc, ClSlash, NP] ** {

  -- based on VS, VQ = Verb ** {c2:Preposition}, VV = Verb ** {c2:Preposition, isAux:Bool}

    -- sentential subject with correlate
  fun
    CorPredSCVP : SC -> VP -> Cl ;       -- it pleases me to swim ; es ist gut , zu schlafen
    CorSCVP : VP -> Cl ;                 -- it always pleases me (= ImpersCl ?)

    -- sentential object with correlate

    CorComplVS : VS -> S -> VP ;         -- (glaube daran | weiß es) , dass S
    CorVS : VS -> VP ;                   -- glaube daran | weiß es

    -- interrogative object with correlate

    CorComplVQ : VQ -> QS -> VP ;        -- frage danach, wann ihr kommt
    CorVQ : VQ -> VP ;                   -- frage danach

    -- infinitival object with correlate (not to be used for modal verbs vv.isAux)

    CorComplVV : VV -> VP -> VP ;        -- versuche es, ein Lied zu singen (was: EsVV)
    CorVV: VV -> VP ;                    -- versuche es|das

    -- -- VS,VQ,VV with nominal object
    -- Compl2VS : VS -> NP -> VP ;          -- hoffe auf den Lottogewinn
    -- Compl2VQ : VQ -> NP -> VP ;          -- wundere mich über deine Frage
    -- Compl2VV : VV -> NP -> VP ;          -- wage einen Versuch [zu unternehmen] (overgenerates!)

    -- Replace the above three rules by the next more general rules (cf. Backward.gf):
    -- e.g. Compl2VS v np = ComplSlash (SlashV2a (UseVS v)) np ; but keep VS.c2 : Preposition!
    UseVS : VS -> V2 ;                   -- know (a secret)
    UseVQ : VQ -> V2 ;                   -- ask (a question) ; frage (nach einer Auskunft)
    UseVV : VV -> V2 ;                   -- try (another method)

    -- for verbs with nominal and sentential|interrogative object with correlate

    CorSlashV2S : V2S -> S -> VPSlash ;  -- überzeuge (ihn) davon , dass sie schlafen
    CorSlashV2Q : V2Q -> QS -> VPSlash ; -- frage (ihn) danach , ob sie schlafen
    CorV2S : V2S -> VPSlash ;            -- überzeuge (ihn) davon
    CorV2Q : V2Q -> VPSlash ;            -- frage (ihn) danach

    CorSlashV2V : V2V -> VP -> VPSlash ; -- bitte (dich) darum , nicht zu schlafen
    CorV2V : V2V -> VPSlash ;            -- bitte (dich) darum ; empfehle es (dir) 

    -- Rem. A nominal object can be added by ComplSlash : VPSlash -> NP -> VP 

    -- To use the verb with nominal instead of sentential object
    UseV2S : V2S -> V3 ;                 -- answer (them) (many questions | what)
    UseV2Q : V2Q -> V3 ;                 -- ask (you) (several questions | what)
    UseV2V : V2V -> V3 ;                 -- promise (you) (my support | what)

  -- Questions with interrogatve sentential pronoun "was" (or correlate "woran" etc.)

  cat
    VPSlashS ;                   -- (NP\Cl)/S, clause missing subject and sentential object
    ClSlashS ;                   -- Cl/S,      clause missing sentential object
    SSlashS ;                    -- S/S,       sentence missing sentential object
  fun
    SlashVSa : VS -> VPSlashS ;                   -- believe (that he lives | what)
    Slash2V2S : V2S -> NP -> VPSlashS ;           -- antworte dir (dass .. | was)
                                                  -- erinnere euch (daran, dass ..)

    AdvVPSlashS : VPSlashS -> Adv -> VPSlashS ;   -- always believe
    SlashVPSlashS : NP -> VPSlashS -> ClSlashS ;  -- John believes (that we sleep)

    QuestSlashS : ClSlashS -> QCl ;               -- what does John believe ; worauf hoffen wir
    RelSlashS : ClSlashS -> RCl ;                 -- what John believes ; worauf wir hoffen

    -- ComplSlashS : VPSlashS -> S -> VP can be simulated by Verb.ComplVS : VS -> S -> VP and
    -- (ComplSlashS (Slash2V2S v2s np) s) := (ComplSlash (SlashV2S v2s s) np) : VP 

    UseSlashS : Temp -> Pol -> ClSlashS -> SSlashS ; -- 

  cat
    VPSlashQS ;                 -- (NP\Cl)/QS, clause missing subject and interrogative object
    ClSlashQS ;                 -- Cl/QS,      clause missing interrogative object
  fun
    SlashVQa : VQ -> VPSlashQS ;                    -- frage (nach NP | ob S | danach | wonach)
    Slash2V2Q : V2Q -> NP -> VPSlashQS ;            -- frage euch 

    AdvVPSlashQS : VPSlashQS -> Adv -> VPSlashQS ;  -- ask repeatedly
    SlashVPSlashQS : NP -> VPSlashQS -> ClSlashQS ; -- John asks (whether we sleep)

    QuestSlashQS : ClSlashQS -> QCl ;               -- what does John ask for ; wonach fragen wir
    RelSlashQS : ClSlashQS -> RCl ;                 -- wonach wir fragen
    
  cat
    VPSlashVP ;                 -- (NP\Cl)/VP, clause missing subject and infinitival object
    ClSlashVP ;                 -- Cl/VP,      clause missing infinitival object
  fun
    SlashVVa : VV -> VPSlashVP ;                    -- denke (daran | woran) [zu schlafen]
    Slash2V2V : V2V -> NP -> VPSlashVP ;
    
    AdvVPSlashVP : VPSlashVP -> Adv -> VPSlashVP ;  -- ask repeatedly
    SlashVPSlashVP : NP -> VPSlashVP -> ClSlashVP ; -- John asks (whether we sleep)

    QuestSlashVP : ClSlashVP -> QCl ;
    RelSlashVP : ClSlashVP -> QCl ;
    
    -- Leftextraction of sentential object and (right) correlate "das", "danach"
    -- for main clauses only, hence resulting in Extra.Foc
    -- TODO: Replace ClSlashS by SSlashS and ClSlashQS by SSlashQS, to allow more tenses
  fun
    FocS : S -> ClSlashS -> Foc ;       -- that John sleeps , they do believe
    FocCorS : S -> ClSlashS -> Foc ;    -- dass er schläft , das glauben sie
    CorFocS : ClSlashS -> Foc ;         -- das glauben sie ; darauf hoffen wir

    FocQS : QS -> ClSlashQS -> Foc ;    -- whether John sleeps , we do not know
    FocCorQS : QS -> ClSlashQS -> Foc ; -- ob er schläft , danach fragen wir (nicht)
    CorFocQS : ClSlashQS -> Foc ;       -- danach fragen wir

    FocVP : VP -> ClSlashVP -> Foc ;    -- to live in peace we try [to]
    FocCorVP : VP -> ClSlashVP -> Foc ; -- im Frieden zu leben , das versuchen wir
    CorFocVP : ClSlashVP -> Foc ;       -- das versuchen wir ; darum bitten wir dich

    ContractedUseFoc : Temp -> Pol -> Foc -> Utt ;  -- for english, order = (ODir True)
    
    -- Sentences in conjunctive mood
  cat
    SConj ;
  fun
    UseClConj : Temp -> Pol -> Cl -> SConj ; -- clause as sentence in conjunctive

    -- Noun with sentential complement (to replace SentCN : CN -> SC -> CN)
    
  cat
    NS ; NQ ; NV ;                    -- assume NS,NQ,NV = Noun ** {c2:Preposition}
  fun                                 -- Ger: N2, for Frage danach , wo wir leben
    ComplNS : NS -> S -> CN ;         -- claim that we sleep
    ComplNSConj : NS -> SConj -> CN ; -- claim we would sleep ; Glaube, wir würden schlafen
    CorComplNS : NS -> S -> CN ;      -- Glaube daran , dass sie schlafen
    Compl2NS : NS -> NP -> CN ;       -- Glaube an den Erfolg
    -- Compl2NS suppresses adverbial attributes AdvCN (UseNS ns) (PrepNP ns.c2 np)
    -- with UseNS : NS -> CN ;   -- Glaube (an den Erfolg)
    CorNS : NS -> CN ;                -- Glaube daran ; eng. belief therein (?)

    ComplNV : NV -> VP -> CN ;        -- Hoffnung , zu überleben
    CorComplNV : NV -> VP -> CN ;     -- Hoffnung darauf , zu überleben
    Compl2NV : NV -> NP -> CN ;       -- Hoffung auf einen Erfolg
    CorNV : NV -> CN ;                -- Hoffnung darauf
    
    ComplNQ : NQ -> QS -> CN ;        -- question where we live
    CorComplNQ : NQ -> QS -> CN ;     -- Frage danach , wo wir leben
    -- Compl2NQ : NQ -> NP -> CN ;       -- Frage nach einem Grund
    CorNQ : NQ -> CN ;                -- Frage danach
    UseNQ : NQ -> N2 ;                -- Frage
    
    -- Adjective with sentential complement (to replace SentAP : AP -> SC -> AP).

  cat
    AS ; AQ ; AV ;
  fun
    ComplAS : AS -> S -> AP ;       -- (we are) glad that they sleep
    ComplAV : AV -> VP -> AP ;      -- (we are) eager to be alive
    ComplAQ : AQ -> QS -> AP ;      -- (we are) uncertain whether they arrived
    -- can't add a comparison np to the AV.comp: (she is) more happy than he to live in Europe
    -- ComplAVComp : AV -> NP -> VP -> AP ; -- AV.s!comp ++ np.s ! av.s2 ++ vp.inf

    CorComplAS : AS -> S -> AP ;     -- (wir sind) froh darüber (, dass sie schlafen)
    CorComplAV : AV -> VP -> AP ;    -- (wir sind) bestrebt (, lange zu leben)
    CorComplAQ : AQ -> QS -> AP ;    -- (wir sind) ungewiß darüber (, wo sie schlafen)

    CorAS : AS -> AP ;               -- (wir sind) froh darüber ; (ein) darüber frohes (Kind)
    CorAV : AV -> AP ;
    CorAQ : AQ -> AP ;
    
    Compl2AS : AS -> NP -> AP ;       -- nominal instead of sentential object
    Compl2AV : AV -> NP -> AP ;
    Compl2AQ : AQ -> NP -> AP ;
    
    -- unsicher
  cat
    VSA ; -- variant of V2A; e.g. finde, dass es schneit, schön
    VVA ; --                 e.g. finde, im See zu schwimmen, schön
  fun
    CorVSA : VSA -> AP -> VP ;           -- finde es|das sehr schön
                                         -- consider it harmless
    ComplVSA : VSA -> S -> AP -> VP ;    -- finde sehr schön, dass es schneit
                                         -- consider harmless that S
    CorComplVSA : VSA -> S -> AP -> VP ; -- finde es sehr schön, dass S;
                                         -- consider it harmless that S
    CorComplVVA : VSA -> VP -> AP -> VP ; -- finde es gut, zu schwimmen

    -- obsolete by CorComplVV resp. CorComplVSA, but keep for backward-compatibility:
	EsVV : VV -> VP -> VP ;        -- ich genieße es zu schlafen
        EsV2A : V2A -> AP -> S -> VP ; -- ich finde es schön, dass ...

}
