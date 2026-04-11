concrete SentenceMkd of Sentence = CatMkd ** open Prelude,ResMkd in {

lin PredVP np vp = {s = mkClause (np.s ! RSubj) np.a vp} ;

    UseCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! Main
    } ;
    UseQCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p
    } ;
    UseRCl t p cl = {
      s = \\gn => t.s ++ p.s ++ cl.s ! gn ! t.t ! t.a ! p.p
    } ;

}
