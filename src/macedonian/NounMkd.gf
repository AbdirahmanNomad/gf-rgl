concrete NounMkd of Noun = CatMkd ** open Prelude,ResMkd in {
  lin AdjCN ap cn = {
       s = case ap.isPre of {
             True  => \\s,n => ap.s ! s
                                    ! case n of {
                                        Sg => GSg cn.g;
                                        Pl => GPl
                                      } ++
                               cn.s ! Indef ! n;
             False => \\s,n => cn.s ! s ! n ++
                               ap.s ! Indef
                                    ! case n of {
                                        Sg => GSg cn.g;
                                        Pl => GPl
                                      }
           };
       vocative = \\n => ap.s ! Indef
                              ! case n of {
                                  Sg => GSg cn.g;
                                  Pl => GPl
                                } ++
                         cn.vocative ! n;
       count_form = ap.s ! Indef ! GPl ++ cn.count_form; g = cn.g} ;
  lin AdvCN cn adv = {
        s = \\s,n => cn.s ! s ! n ++ adv.s;
        count_form = cn.count_form ++ adv.s;
        vocative = \\n => cn.vocative ! n ++ adv.s;
        g = cn.g
      } ;
  lin DefArt = {s = []; sp = Def Unspecified} ;
  lin DetCN det cn = {
        s = \\r => case det.n of {
                     NNum n     => det.s ++ cn.s ! det.sp ! n;
                     NCountable => det.s ++ cn.count_form
                   } ;
        vocative = case det.n of {
                     NNum Sg => det.s ++ cn.vocative ! Sg;
                     _       => det.s ++ cn.vocative ! Pl
                   } ;
        a = {g = case det.n of {
                   NNum Sg => GSg cn.g;
                   _       => GPl
                 } ;
             p = P3};
      } ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumPl = {s = []; n = NNum Pl} ;
  lin NumSg = {s = []; n = NNum Sg} ;
  lin UseN s = s ;
  lin UsePron p = p ** {vocative=p.s ! RSubj} ;

  lin RelNP np rs = {
        s = \\r => np.s ! r ++ rs.s ! np.a.g ;
        vocative = np.vocative ++ rs.s ! np.a.g ;
        a = np.a;
      } ;
  lin RelCN cn rs = {
        s = \\sp,n => cn.s ! sp ! n ++ rs.s ! genNum cn.g n ;
        count_form = cn.count_form ++ rs.s ! GPl ;
        vocative = \\n => cn.vocative ! n ++ rs.s ! genNum cn.g n ;
        g = cn.g;
      } ;
}
