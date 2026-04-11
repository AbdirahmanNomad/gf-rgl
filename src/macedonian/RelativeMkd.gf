concrete RelativeMkd of Relative = CatMkd ** open Prelude,ResMkd in {
  lin IdRP = {s = table {
                    GSg Masc => "кој" ;
                    GSg Fem => "која" ;
                    GSg Neuter => "кое" ;
                    GPl => "кои"
                  }
             } ;
  lin RelVP rp vp = {
        s = \\gn,t,a,p => mkClause (rp.s ! gn) {g=gn; p=P3} vp ! t ! a ! p ! Main
      } ;
}
