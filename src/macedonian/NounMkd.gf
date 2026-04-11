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
  lin DefArt = {s = \\_=>[]; sp = Def Unspecified} ;
  lin DetCN det cn = {
        s = \\r => case det.n of {
                     NNum n     => det.s ! cn.g ++ cn.s ! det.sp ! n;
                     NCountable => det.s ! cn.g ++ cn.count_form
                   } ;
        vocative = det.s ! cn.g ++ cn.vocative ! nnum2num det.n ;
        a = {g = genNum cn.g (nnum2num det.n) ;
             p = P3};
      } ;
  lin DetQuant q num = {s = \\g => q.s ! genNum g (nnum2num num.n) ++ num.s;
                        n = num.n;
                        sp = q.sp} ;
  lin IndefArt = {s = \\_=>[]; sp = Indef} ;
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
