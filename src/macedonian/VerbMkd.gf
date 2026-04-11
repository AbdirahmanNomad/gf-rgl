concrete VerbMkd of Verb = CatMkd ** open Prelude,ResMkd in {

  lin UseV v = v ** {compl = \\_ => []};
  lin ComplSlash vp np = vp ** {compl = \\a => vp.compl ! a ++ vp.c2.s ++ np.s ! RObj vp.c2.c} ;
  lin SlashV2a v = v ** {compl = \\_ => []} ;

}
