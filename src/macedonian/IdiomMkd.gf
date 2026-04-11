concrete IdiomMkd of Idiom = CatMkd ** open Prelude,ResMkd in {

lin ProgrVP vp = vp ** {
       present = \\a,n,p => vp.present ! Imperfective ! n ! p ;
       imperfect = \\a,n,p => vp.imperfect ! Imperfective ! n ! p ;
       imperative = \\a,n => vp.imperative ! Imperfective ! n ;
       participle = {aorist = \\a,gn => vp.participle.aorist ! Imperfective ! gn ;
                     perfect = \\a => vp.participle.perfect ! Imperfective ;
                     imperfect = \\a => vp.participle.imperfect ! Imperfective ;
                     adjectival = vp.participle.adjectival ;
                     adverbial = vp.participle.adverbial}
    } ;

}
