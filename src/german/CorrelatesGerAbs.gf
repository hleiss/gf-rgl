--# -path=.:../abstract:../common:../api:../prelude

abstract CorrelatesGerAbs = Cat ** {

    -- sentential subject with correlate
  fun
    CorPredSCVP : SC -> VP -> Cl ;       -- it happens often that I sleep
    CorSCVP : VP -> Cl ;                 -- it happens rarely (= ImpersCl ?)

    -- sentential ir object with correlate (for VS,VQ = Verb ** {c2:Preposition} )

    CorComplVS : VS -> S -> VP ;         -- glaube daran|es, dass S
    CorVS : VS -> VP ;                   -- glaube daran|es

    -- interrogative object with correlate

    CorComplVQ : VQ -> QS -> VP ;        -- frage danach, wann ihr kommt
    CorVQ : VQ -> VP ;                   -- frage es|danach

    -- infinitival object with correlate (not to be used for modal verbs vv.isAux)

    CorComplVV : VV -> VP -> VP ;        -- versuche es, ein Lied zu singen (was: EsVV)
    CorVV: VV -> VP ;                    -- versuche es|das

    -- -- VS,VQ,VV with nominal object
    -- Compl2VS : VS -> NP -> VP ;          -- hoffe auf den Lottogewinn
    -- Compl2VQ : VQ -> NP -> VP ;          -- wundere mich über deine Frage
    -- Compl2VV : VV -> NP -> VP ;          -- wage einen Versuch [zu unternehmen] (overgenerates!)

    -- Replace the above three rules by the next more general rules (cf. Backward.gf):
    -- e.g. Compl2VS v np = ComplSlash (SlashV2a (UseVS hope_VS)) ; but keep VS.c2 : Preposition!
    UseVS : VS -> V2 ;                   -- know (a secret)
    UseVQ : VQ -> V2 ;                   -- ask (a question) ; frage (nach einer Auskunft)
    UseVV : VV -> V2 ;                   -- try (another method)

    -- for verbs with nominal and sentential|interrogative object with correlate

    CorSlashV2S : V2S -> S -> VPSlash ;  -- überzeuge (ihn) davon , dass die Sonne scheint
    CorSlashV2Q : V2Q -> QS -> VPSlash ; -- frage (ihn) danach , ob die Sonne scheint
    CorV2S : V2S -> VPSlash ;            -- überzeuge (ihn) davon
    CorV2Q : V2Q -> VPSlash ;            -- frage (ihn) danach

    CorSlashV2V : V2V -> VP -> VPSlash ; -- bitte (dich) darum , nicht zu schlafen
    CorV2V : V2V -> VPSlash ;            -- bitte (dich) darum

    -- Rem. A nominal object can be added by ComplSlash : VPSlash -> NP -> VP 

    -- To use with nominal instead of sentential object
    UseV2S : V2S -> V3 ;                -- answer (them) (many questions | what)
    UseV2Q : V2Q -> V3 ;                -- ask (you) (several questions | what)
    UseV2V : V2V -> V3 ;                -- promise (you) (my support | what)

  -- Questions with interrogatve sentential pronoun "was" (or correlate "woran" etc.)

  cat
    VPSlashS ;                   -- (NP\Cl)/S, clause missing subject and sentential object
    ClSlashS ;                   -- Cl/S,      clause missing sentential object
  fun
    SlashVSa : VS -> VPSlashS ;                   -- believe (that he lives | in your capabilities)
    SlashVQa : VQ -> VPSlashS ;                   -- fragen (nach NP | ob S | danach | wonach)
    SlashVVa : VV -> VPSlashS ;                   -- denke (daran | woran) [zu schlafen]

    AdvVPSlashS : VPSlashS -> Adv -> VPSlashS ;   -- believe strongly
    SlashVPSlashS : NP -> VPSlashS -> ClSlashS ;  -- John believes (that we sleep)
    QuestSlashS : ClSlashS -> QCl ;               -- what does John believe ; worauf hoffen wir
    -- RelSlashS : IP -> ClSlashS -> RCl ;           -- what John believes ; worauf wir hoffen

    Slash2V2S : V2S -> NP -> VPSlashS ;            -- dir antworten (dass .. | was)
    ComplSlashS : VPSlashS -> S -> VP ;            -- dir antworten , dass wir leben
                                                   --   Todo: remove ComplVS : VS -> S -> VP
    -- Sentences in conjunctive mood
  cat
    SConj ;
  fun
    UseConjCl : Temp -> Pol -> Cl -> SConj ; -- clause as sentence in conjunctive

    -- Noun with sentential complement (to replace SentCN : CN -> SC -> CN)
  cat
    NS ; NQ ; NV ;                    -- can be Noun if sc-complement goes to N.ext
  fun                                 -- Ger: N2, for Frage danach , wo wir leben
    UseNS : NS -> CN ;                -- claim
    ComplNS : NS -> S -> CN ;         -- claim that we sleep
    ComplConjNS : NS -> SConj -> CN ; -- claim we would sleep
    CorNS : NS -> CN ;                -- Glaube daran
    CorComplNS : NS -> S -> CN ;      -- Glaube daran , dass sie schlafen
    Compl2NS : NS -> NP -> CN ;       -- Glaube an den Erfolg

    UseNV : NV -> CN ;                -- Hoffnung
    ComplNV : NV -> VP -> CN ;        -- Hoffnung , zu überleben
    Compl2NV : NV -> NP -> CN ;       -- Hoffung auf einen Erfolg
    CorNV : NV -> CN ;                -- Hoffnung darauf
    CorComplNV : NV -> VP -> CN ;     -- Hoffnung darauf , zu überleben

    UseNQ : NQ -> CN ;                -- question
    ComplNQ : NQ -> QS -> CN ;        -- question where we live
    Compl2NQ : NQ -> NP -> CN ;       -- Frage nach einem Grund
    CorNQ : NQ -> CN ;                -- Frage danach
    CorComplNQ : NQ -> QS -> CN ;     -- Frage danach , wo wir leben

{-    -- simpler alternative for binary noun with sentential object
  fun
    SentN2 : N2 -> SC -> CN ;
    SentCorN2 : N2 -> SC -> CN ;
    UseN2 : N2 -> CN ;
    CorN2 : N2 -> CN ;

-}

    -- Adjective with sentential complement (to replace SentAP : AP -> SC -> AP).
{-
  cat
    AS ; AQ ; AV ;
  fun
    ComplAS : AS -> S -> AP ;       -- (we are) glad that they sleep  froh_über:A2=AV
    ComplAV : AV -> VP -> AP ;      -- (we are) happy to be alive
    ComplAQ : AQ -> QS -> AP ;      -- (we are) uncertain whether they arrived
    -- can't add a comparison np to the AV.comp: (she is) more happy than he to live in Europe
    -- ComplAVComp : AV -> NP -> VP -> AP ; -- AV.s!comp ++ np.s ! av.s2 ++ vp.inf
-}
    -- simpler alternative for binary adjective with sentential object
  fun
    SentA2 : A2 -> SC -> AP ;     -- begierig , etwas zu tun
    CorSentA2 : A2 -> SC -> AP ;  -- begierig darauf , etwas zu tun
    CorA2 : A2 -> AP ;            -- begierig darauf

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
