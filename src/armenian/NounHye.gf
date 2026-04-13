concrete NounHye of Noun = CatHye ** open ResHye in {
  lin AdvCN cn adv = {
        s = \\sp,c,num => cn.s ! sp ! c ! num ++ adv.s ;
        poss1 = \\c,num => cn.poss1 ! c ! num ++ adv.s ;
        poss2 = \\c,num => cn.poss2 ! c ! num ++ adv.s
      } ;
  lin DefArt = {s = []; sp = Def} ;
  lin DetCN det cn = {s = \\c => det.s ++ cn.s ! det.sp ! c ! det.n;
                      a = {n=det.n; p=P3}} ;
  lin DetQuant quant num = {s = quant.s ++ num.s; n = num.n; sp=quant.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin PossPron pron = {s = pron.empty; sp = Poss pron.a.p} ;
  lin UseN n = n ** {
        s = \\sp,c,num =>
                case <sp,c> of {
                  <Def,Nom> => n.def_nom ! num ;
                  <Def,Dat> => n.def_dat ! num ;
                  <Poss P1,_> => n.poss1 ! c ! num ;
                  <Poss P2,_> => n.poss2 ! c ! num ;
                  _         => n.s ! c ! num
                }
      } ;
  lin UsePron pron = {s = \\c => pron.s; a=pron.a} ;
}
