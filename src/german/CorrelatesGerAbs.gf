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
    CorVS : VS -> VP ;                   -- frage es|danach

    -- infinitival object with correlate (not to be used for modal verbs vv.isAux)

    CorComplVV : VV -> VP -> VP ;        -- versuche es, ein Lied zu singen (was: EsVV)
    CorVV: VV -> VP ;                    -- versuche es|das

    -- for verbs with nominal and sentential|interrogative object with correlate

    CorSlashV2S : V2S -> S -> VPSlash ;   -- überzeuge (ihn) davon , dass die Sonne scheint
    CorSlashV2Q : V2Q -> QS -> VPSlash ;  -- frage (ihn) danach , ob die Sonne scheint
    CorV2S : V2S -> VPSlash ;             -- überzeuge (ihn) davon
    CorV2Q : V2Q -> VPSlash ;             -- frage (ihn) danach

    CorSlashV2V : V2V -> VP -> VPSlash ;  -- bitte (dich) darum , nicht zu schlafen
    CorV2V : V2V -> VPSlash ;             -- bitte (dich) darum

{-  With ComplSlash : VPSlash -> NP -> VP, the following can be derived

    -- nominal object plus sentential or interrogative object with correlate 

    ComplSlash (CorSlashV2S v s) np      -- überzeuge ihn davon , dass ...
    ComplSlash (CorSlashV2Q v q) np      -- frage ihn danach, wann ihr kommt
    ComplSlash (CorV2S v) np             -- überzeuge ihn davon
    ComplSlash (CorV2Q v) np             -- frage ihn danach
                                              -- pron.switch: antworte es ihm ?

    -- nominal object plus infinitival object with correlate 

    ComplSlash (CorSlashV2V v vp) np     -- bitte dich darum , zu arbeiten
    ComplSlash (CorV2V v) np             -- bitte dich darum
-}

-- Interrogatve correlate for sentential object
-- ICorVS : VS -> QVP ;          -- woran glauben
-- PredIQVP : QVP -> NP -> QCl ; -- woran glauben die Kinder

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
