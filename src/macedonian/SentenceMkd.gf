concrete SentenceMkd of Sentence = CatMkd ** open Prelude,ResMkd in {

lin PredVP np vp =
        let n = case np.g of {
                  GSg _ => Sg ;
                  GPl   => Pl
                }
        in {s = \\t,a,p,o =>
                    case <t,a> of {
                      <VPresent,Simul> => np.s ! RSubj ++ neg ++ vp.present ! Imperfective ! n ! np.p ;
                      <VPresent,Anter> => np.s ! RSubj ++ neg ++ auxBe.present ! n ! np.p ++ vp.participle.imperfect ! Perfective ! np.g ;
                      <VPastSimple,Simul> => np.s ! RSubj ++ neg ++ vp.aorist ! n ! np.p ;
                      <VPastSimple,Anter> => np.s ! RSubj ++ neg ++ auxBe.imperfect ! n ! np.p ++ vp.participle.imperfect ! Perfective ! np.g ;
                      <VPastImperfect,Simul> => np.s ! RSubj ++ neg ++ vp.imperfect ! Perfective ! n ! np.p ;
                      <VPastImperfect,Anter> => np.s ! RSubj ++ neg ++ auxBe.imperfect ! n ! np.p ++ vp.participle.imperfect ! Perfective ! np.g ;
                      <VFut, Simul> => np.s ! RSubj ++ fut ++ vp.present ! Perfective ! n ! np.p ;
                      <VFut, Anter> => np.s ! RSubj ++ fut ++ auxHave.present ! n ! np.p ++ vp.participle.perfect ! Perfective ;
                      <VCond,Simul> => np.s ! RSubj ++ neg ++ "би" ++ vp.participle.imperfect ! Perfective ! np.g ;
                      <VCond,Anter> => np.s ! RSubj ++ neg ++ "би" ++ auxHave.participle.imperfect ! np.g ++ vp.participle.perfect ! Perfective
                    } where {
                        neg = case p of {
                                Pos => "" ;
                                Neg => "не"
                              } ;
                        fut = case p of {
                                Pos => "ке" ;
                                Neg => "нема да"
                              } ;
                        li  = case o of {
                                Main  => "" ;
                                Quest => "ли"
                              }
                    }
           } ;

    UseCl t p cl = {
      s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! Main
    } ;

}
