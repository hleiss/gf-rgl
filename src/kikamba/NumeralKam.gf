concrete NumeralKam of Numeral = CatKam [Numeral,Digits] ** open Prelude,CommonBantu,DiffKam, MorphoKam in {

lincat 
  Digit = {s : DForm => CardOrd => Gender => Str} ;
  Sub10 = {s : DForm => CardOrd => Gender => Str ; n : Number} ;
  Sub100     = {s : CardOrd => Gender => Str ; n : Number} ;
  Sub1000    = {s : CardOrd => Gender => Str ; n : Number} ;
  Sub1000000 = {s : CardOrd => Gender => Str ; n : Number} ;

lin num x = x ;
lin n2 = mkNumn "li"   "ili"    "eli" "keli" ;
lin n3 = mkNum "tatu" "itatu"  " atatu" "katatu" ;
lin n4 = mkNum "nya"  "ina"   "ana" "kana" ;
lin n5 = mkNum "tano"  "itano"  "atano" "katano" ;
lin n6 = regNum "nthathatu" ;
lin n7 = regNum "muonza" ;
lin n8 = regNum "nyanya" ;
lin n9 = regNum "kenda" ;

lin pot01 = mkNume "mwe"  " yimwe" "mbee" ** {n = Sg} ;
lin pot0 d = d ** {n = Pl} ;
lin pot110 = regCardOrd "ikumi" ** {n = Pl} ;
lin pot111 = regCardone "ikumi na" "mwe" ** {n = Pl} ; 
lin pot1to19 d = {s = d.s ! teen} ** {n = Pl} ;
lin pot0as1 n = {s = n.s ! unit}  ** {n = n.n} ;
lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
lin pot1plus d e = { s = table {
      NCard => \\g => d.s ! ten ! NCard ! g ++ "na"++ e.s ! unit ! NCard ! g  ;
      NOrd => \\g =>Ordprefix g++ d.s ! ten ! NCard ! g ++ "na"++ e.s ! unit ! NCard ! g } ;
                  n = Pl} ;
lin pot1as2 n = n ;
lin pot2 d = {s = d.s ! hund} ** {n = Pl} ;	
lin pot2plus d e = {s = table {
      NCard => \\g => d.s ! hund ! NCard ! g ++  "na" ++ e.s !NCard ! g ;
      NOrd => \\g =>Ordprefix g++ d.s ! hund ! NCard ! g ++  "na" ++ e.s ! NCard ! g } ;
                   n = Pl} ;
 lin pot2as3 n = n ;
lin pot3 n = { s = table {
      NCard => \\g => mkCard NCard "ngili" ! g ++ n.s ! NCard ! g ;
      NOrd => \\g =>Ordprefix g++ mkCard NCard "ngili" ! g ++ n.s ! NCard ! g } ;
              n = Pl} ;
lin pot3plus n m = { s = table {
      NCard => \\g => "ngili" ++ n.s ! NCard !g ++  m.s ! NCard ! g ;
      NOrd => \\g =>Ordprefix g++ "ngili" ++ n.s ! NCard !g ++  m.s ! NCard ! g} ;
                 n = Pl} ;

-- numerals as sequences of digits0'

  lincat 
    Dig = TDigit ;

  lin
    IDig d = d ; 

    IIDig d i = {
            s = table {NCard => \\g => d.s! NCard ! g ++ BIND  ++ i.s ! NCard  ! g ;
                 NOrd => \\g => d.s! NOrd! g ++ BIND  ++ i.s !NCard! g } ;
           n = Pl 
    } ;

    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1" Sg ;
    D_2 = mkDig "2" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

    PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {
      s = \\o,g => "-" ++ BIND  ++ d.s ! o ! g ;
      n = Pl ;
      hasDot=False
      } ;
    IFrac d i = {
     s = \\o,g => d.s ! NCard ! g ++
                  if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
                  i.s ! o ! g ;
     n = Pl ;
     hasDot=True
     } ;

  oper
    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
    mkDig : Str -> TDigit = \c -> mk2Dig c (c ) ;

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard => \\g => c ; NOrd => \\g =>Ordprefix g ++ o} ; --Ordprefix g ++
      n = n} ;

    TDigit = {
      n : Number ;
      s : CardOrd => Gender => Str
    } ;

}
